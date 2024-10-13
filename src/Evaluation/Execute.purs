module Miros.Execution (compile, resolveImport) where

import Miros.Prelude

import Data.Array as Array
import Data.HashMap as HM
import Data.HashSet as HS
import Data.List as List
import Data.String as String
import Miros.Ast as Ast
import Miros.Evaluation.EscapeAlternatingExpansion as EAE
import Miros.Evaluation.EvaluationContext (BoundVariable, Context, DBLevel, NormalizedSnippet, Worldline, expandReferences, references)
import Miros.Evaluation.Expansion (Expansion, ExpansionChunk(..))
import Miros.Evaluation.Expansion as EX
import Miros.Parser.Implementation (parseToplevel)
import Miros.Parser.Lib (runParser)
import Miros.Parser.Pieces (eof, inscribe, optWs, runes)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath, dirname)

evalExpression
  :: Boolean
  -> Context
  -> Worldline
  -> Ast.Expr
  -> SteinerM Expansion
evalExpression expandArrays context worldline = loopUnescaped
  where
  -- | Lookup the value of an evaluated variable in scope
  lookup var = do
    level <- liftMaybe ("Undefined variable @" <> var)
      $ HM.lookup var context.nameMap
    liftMaybe ("Wordline did not collapse @" <> var)
      $ join
      $ Array.index worldline level

  loopUnescaped :: Ast.Expr -> SteinerM Expansion
  loopUnescaped expr = do
    eae <- loop expr
    liftMaybe ("Invalid escape in expression " <> pretty expr) $ EAE.asUnescaped eae

  loop :: Ast.Expr -> SteinerM EAE.EscapeAlternatingExpansion
  loop = case _ of
    Ast.Empty -> pure mempty
    Ast.Newline -> pure $ EAE.singleton $ Literal "\n"
    Ast.Literal literal -> pure $ EAE.singleton $ Literal literal
    Ast.TabStop i -> pure $ EAE.singleton $ TabStop i
    Ast.CaptureGroupRef i -> pure $ EAE.singleton $ CaptureGroupRef i
    Ast.Var var -> lookup var <#> _.value <#> EAE.unescaped
    Ast.ArrayIndex name arr -> do
      evaluatedVar <- lookup name
      expr <- liftMaybe ("Empty array to index by variable @" <> name)
        $ Array.index arr (evaluatedVar.index `mod` Array.length arr)
      loopUnescaped expr <#> EAE.unescaped
    Ast.Array arr ->
      if expandArrays then
        traverse loopUnescaped arr
          <#> lift
          # join
          <#> EAE.unescaped
      else
        traverse loopUnescaped arr
          <#> Array
          <#> EAE.singleton
    Ast.Concat arr -> traverse loop arr <#> fold
    Ast.Choice index arr ->
      traverse loopUnescaped arr
        <#> Choice index
        <#> EAE.singleton
    Ast.Nonempty index inner ->
      loop inner
        <#> EAE.collapse (Nonempty index)
        <#> EAE.unescaped
    Ast.Escape inner ->
      loopUnescaped inner <#> EAE.escaped

expansionAsString
  :: forall m
   . MonadThrow String m
  => EX.Expansion
  -> m String
expansionAsString e = traverse go (EX.chunks e) <#> fold
  where
  go :: EX.ExpansionChunk -> m String
  go = case _ of
    EX.Literal l -> pure l
    _other -> throwError $ "Cannot convert complex expressions to string: " <>
      pretty e

-- | Basic monad stack which can error out and branch out.
type SteinerM = ExceptT String Array

-- | *Untear a rift separating two far away lands, combining their results*
-- |
-- | This helper joins the underlying arrays of two computations.
untear :: forall a. SteinerM a -> SteinerM a -> SteinerM a
untear = over2 ExceptT (<>)

compile :: Ast.Scope -> Array (Either String NormalizedSnippet)
compile scope = runExceptT do
  normalized <- go
    (List.fromFoldable scope)
    { scope: mempty, nameMap: HM.empty, modifiers: HM.empty }
  applySpecialModifiers normalized
  where
  go :: List Ast.Toplevel -> Context -> SteinerM NormalizedSnippet
  go (head : tail) context = case head of
    Ast.Snippet snip -> do
      let
        handleError err =
          throwError $ fold
            [ "In snippet "
            , pretty snip.name
            , ":\n"
            , indentString 2 err
            ]
      flip untear (go tail context) $ flip catchError handleError do
        let
          dependencies =
            HS.unions
              [ maybe HS.empty references snip.description
              , maybe HS.empty references snip.name
              , references snip.expansion
              , references snip.trigger
              ]
              # expandReferences context

          loop :: DBLevel -> Worldline -> BoundVariable -> SteinerM Worldline
          loop level worldline variable =
            if HS.member level dependencies then do
              boundBy <- evalExpression false context worldline variable.boundBy

              let
                choices = case EX.asChunk boundBy of
                  Just (EX.Array paths) -> paths
                  _ -> [ boundBy ]

              index /\ choice <- lift $ Array.mapWithIndex (/\) choices
              pure $ Array.snoc worldline $ Just
                { index
                , source: variable.boundBy
                , value: choice
                }
            else pure $ Array.snoc worldline Nothing

        worldline <- foldWithIndexM loop [] context.scope
        name <- for snip.name
          (evalExpression true context worldline >=> expansionAsString)
        description <- for snip.description
          (evalExpression true context worldline >=> expansionAsString)
        trigger <- evalExpression true context worldline snip.trigger
          >>= expansionAsString
        expansion <- evalExpression true context worldline snip.expansion

        pure
          { name: fromMaybe trigger name
          , description
          , trigger
          , triggerKind: snip.triggerKind
          , expansion
          , modifiers: context.modifiers
          }

    Ast.Block block -> do
      let
        deeper = go (List.fromFoldable block.elements) context
          { modifiers = context.modifiers
              # HM.union (HM.fromArrayBy identity (const true) block.modifiers.positive)
              # HM.union (HM.fromArrayBy identity (const false) block.modifiers.negative)
          }

      -- We concatenate the underlying arrays
      untear
        deeper
        (go tail context)

    Ast.For name expression -> go tail context
      { nameMap =
          HM.insert
            name
            (Array.length context.scope)
            context.nameMap
      , scope = Array.snoc context.scope
          { references: expandReferences context (references expression)
          , boundBy: expression
          }
      }

    Ast.Import path -> do
      throwError $ fold
        [ "Import hasn't been resolved before compilation: "
        , pretty path
        ]

  go Nil _ = lift []

-- | Certain modifiers like `capitalize` modify the behaviour of snippet 
-- | generation. This function applies such logic. For instance, in the case of 
-- | `capitalize`, this function creates another copy of the snippet which has
-- | the first character capitalized.
applySpecialModifiers :: NormalizedSnippet -> SteinerM NormalizedSnippet
applySpecialModifiers = handleCapitalize
  where
  handleCapitalize snippet = case HM.lookup "capitalize" snippet.modifiers of
    Just true | snippet.triggerKind == Ast.String ->
      lift
        [ snippet { modifiers = modifiers }
        , snippet
            { trigger = capitalize snippet.trigger
            , expansion = capitalizeExpansion snippet.expansion
            , modifiers = modifiers
            }
        ]
      where
      modifiers = HM.delete "capitalize" snippet.modifiers
      capitalize =
        runes
          >>> Array.modifyAt 0 String.toUpper
          >>> foldMap inscribe

      -- Cannot use pointfree notation because of recursion
      capitalizeExpansion :: Expansion -> Expansion
      capitalizeExpansion e = e
        # EX.chunks
        # Array.modifyAt 0 capitalizeLiteral
        # fold
        # Array.foldMap EX.singleton

      capitalizeLiteral :: ExpansionChunk -> ExpansionChunk
      capitalizeLiteral (Literal l) = Literal $ capitalize l
      capitalizeLiteral (Nonempty ix e) = Nonempty ix $ capitalizeExpansion e
      capitalizeLiteral (Choice ix es) = Choice ix $ capitalizeExpansion <$> es
      capitalizeLiteral (Array es) = Array $ capitalizeExpansion <$> es
      capitalizeLiteral other = other

    _ -> pure snippet

-- | Recursively resolve imports until the structure has been flattened.
resolveImport :: FilePath -> FilePath -> ExceptT String Aff Ast.Scope
resolveImport basePath path = do
  let
    fullPath = basePath <> "/" <> path <>
      if endsWith ".miros" path then ""
      else ".miros"
    handleError err =
      throwError $ fold
        [ "An error occurred while processing "
        , fullPath
        , "\n"
        , indentString 2 err
        ]

  flip catchError handleError do
    contents <- liftAff $ readTextFile UTF8 $ fullPath

    parsed <- liftEither
      $ lmap pretty
      $ runParser contents (parseToplevel <* optWs <* eof)

    let
      go :: Ast.Toplevel -> ExceptT String Aff Ast.Toplevel
      go (Ast.Import importPath) = do
        scope <- resolveImport (dirname fullPath) importPath
        pure $ Ast.Block { elements: scope, modifiers: mempty }
      go (Ast.Block block) = do
        elements <- traverse go block.elements
        pure $ Ast.Block $ block { elements = elements }
      go other = pure other

    traverse go parsed
