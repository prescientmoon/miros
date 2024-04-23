module Miros.Execution (compile) where

import Miros.Prelude

import Data.Array as Array
import Data.HashMap as HM
import Data.HashSet as HS
import Data.List as List
import Miros.Ast as Ast
import Miros.Evaluation.EscapeAlternatingExpansion as EAE
import Miros.Evaluation.Expansion (Expansion, ExpansionChunk(..))
import Miros.Evaluation.Expansion as EX
import Miros.Evaluation.EvaluationContext
  ( BoundVariable
  , Context
  , DBLevel
  , NormalizedSnippet
  , Worldline
  , expandReferences
  , references
  )

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
    other -> throwError $ "Cannot convert complex expressions to string: " <>
      pretty e

-- | Basic monad stack which can error out and branch out.
type SteinerM = ExceptT String Array

-- | *Untear a rift separating two far away lands, combining their results*
-- |
-- | This helper joins the underlying arrays of two computations.
untear :: forall a. SteinerM a -> SteinerM a -> SteinerM a
untear = over2 ExceptT (<>)

compile :: Ast.Scope -> Array (Either String NormalizedSnippet)
compile scope = runExceptT $ go
  (List.fromFoldable scope)
  { scope: mempty, nameMap: HM.empty, modifiers: HM.empty }
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
  go Nil _ = lift []

