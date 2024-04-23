module Miros.Parser.Implementation
  ( ExprContext(..)
  , parseToplevel
  , parseMultilineExpression
  ) where

import Miros.Prelude

import Data.Array as Array
import Data.List as List
import Miros.Ast (Expr(..), Scope, Snippet, SnippetCommand(..), Toplevel(..), Trigger, TriggerKind(..), makeSnippet)
import Miros.Helpers.Array as Miros.Array
import Miros.Parser.Debug as PD
import Miros.Parser.Lib as P
import Miros.Parser.Pieces as PP

data ExprContext = ArrayHead | ArrayTail | Inline | None

derive instance Eq ExprContext

-- {{{ Basic expression helpers
concatExpressions :: forall f. Foldable f => f Expr -> Expr
concatExpressions = Array.fromFoldable
  >>> case _ of
    [] -> Empty
    [ one ] -> one
    lots -> Concat lots

-- }}}
-- {{{ Atoms
parseTabstop :: P.Parser Expr
parseTabstop = do
  PP.expect "$"
  index <- PP.digit
  pure $ TabStop index

parseVariableName :: P.Parser String
parseVariableName = PP.takeWhile1 "variable name" $ PP.regex "[a-zA-Z0-9]"

parseVariable :: P.Parser Expr
parseVariable = do
  PP.expect "@"
  Var <$> parseVariableName

parseCaptureGroupRef :: P.Parser Expr
parseCaptureGroupRef = do
  PP.expect "@"
  d <- PP.digit
  pure $ CaptureGroupRef d

-- }}}
-- {{{ Single-child expressions
parseChildExpression :: P.Parser Expr
parseChildExpression = PP.eatEol >>=
  if _ then P.agt $ parseMultilineExpression None
  else parseExpression None

-- {{{ Nonempty
parseNonempty :: P.Parser Expr
parseNonempty = P.label "nonempty expression" do
  PP.string "$?"
  index <- PP.digit
  PP.expect "⟨"
  nested <- parseChildExpression
  P.agte $ PP.expect "⟩"
  pure $ Nonempty index nested

-- }}}
-- {{{ Escape
parseEscape :: P.Parser Expr
parseEscape = P.label "escape expression" do
  PP.string "@^⟨"
  nested <- parseChildExpression
  P.agte $ PP.expect "⟩"
  pure $ Escape nested

-- }}}
-- }}}
-- {{{ Multi-child expresions 
parseChildExpressions :: String -> P.Parser (List Expr)
parseChildExpressions name = P.label (name <> " expression body") do
  let mkChild = P.label (name <> "choice expression child")
  PP.eatEol >>=
    if _ then
      P.agt $ PP.sepBy "," $ mkChild $ P.absolute $ parseMultilineExpression ArrayTail
    else
      PP.sepBy "," $ mkChild $ parseExpression ArrayTail

-- {{{ Choice 
parseChoice :: P.Parser Expr
parseChoice = P.label "choice expression" $ PD.do
  PP.string "$|"
  index <- PP.digit
  PP.expect "⟨"
  children <- parseChildExpressions "choice"
  P.agte $ PP.expect "⟩"
  pure $ Choice index $ Array.fromFoldable children

-- }}}
-- {{{ Array (indices)
parseArray :: P.Parser Expr
parseArray = P.label "array expression" PD.do
  PP.string "@⟨"
  contents <- PP.isEol >>=
    if _ then do
      elements <- parseChildExpressions "array"
      pure $ Array $ Array.fromFoldable elements
    else do
      head <- parseExpression ArrayHead
      P.peek >>= \c -> case head, c of
        Var name, Just ":" -> do
          void P.next
          elements <- parseChildExpressions "array index"
          pure $ ArrayIndex name $ Array.fromFoldable elements
        _, Just "," -> do
          void P.next
          tail <- PP.sepBy "," $ parseExpression ArrayTail
          pure $ Array $ Array.fromFoldable $ head : tail
        _, _ -> pure $ Array [ head ]
  P.agte $ PP.expect "⟩"
  pure contents

-- }}}
-- }}}
-- {{{ Expressions
parseExpression :: ExprContext -> P.Parser Expr
parseExpression context = P.label "expression" do
  PP.many chunk <#> concatExpressions
  where
  lit i l = commit $ P.token $ P.nextMany i $> Literal l
  commit parser = Just <$> parser

  chunk = P.label "expression chunk" do
    P.peekMany 2 >>= List.fromFoldable >>> case _ of
      "$" : "?" : _ -> commit parseNonempty
      "$" : "|" : _ -> commit parseChoice
      "$" : n : _
        | PP.isDigit n -> commit parseTabstop
        | otherwise -> lit 1 "$"
      "@" : "⟨" : _ -> commit parseArray
      "@" : "^" : _ -> commit parseEscape
      "@" : n : _
        | PP.isDigit n -> commit parseCaptureGroupRef
        | otherwise -> commit parseVariable
      "\\" : "," : _ -> lit 2 ","
      "\\" : ":" : _ -> lit 2 ":"
      "\\" : "⟩" : _ -> lit 2 "⟩"
      "\\" : "⟨" : _ -> lit 2 "⟨"
      "\\" : "@" : _ -> lit 2 "@"
      "\\" : "$" : _ -> lit 2 "$"
      "\\" : "⋄" : _ -> lit 2 "⋄"
      "\\" : "\\" : _ -> lit 2 "\\"
      "\\" : _ -> lit 1 "\\"
      "⋄" : _ -> lit 1 ""
      "⟩" : _ -> pure Nothing
      "\n" : _ -> pure Nothing
      "," : _ | context == ArrayTail || context == ArrayHead -> pure Nothing
      ":" : _ | context == ArrayHead -> pure Nothing
      " " : _ | context == Inline -> pure Nothing
      Nil -> pure Nothing
      head : _ -> commit $ P.token do
        void $ P.next
        tail <- PP.takeWhile $ flip Array.notElem
          [ "\\", "@", "⟩", "$", "\n", "⋄", " ", ",", ":" ]
        pure $ Literal $ head <> tail

-- }}}
-- {{{ Multline expressions
parseMultilineExpression :: ExprContext -> P.Parser Expr
parseMultilineExpression context = P.label "multiline expression" do
  loop Nothing
    <#> Array.fromFoldable
    <#> Array.dropWhile ((==) Empty)
    <#> Miros.Array.dropEndWhile ((==) Empty)
    <#> Array.intersperse Newline
    <#> Array.filter ((/=) Empty)
    <#> concatExpressions
  where
  loop ws = PD.do
    case ws of
      Just amount -> PP.exactWhitespace amount
      Nothing -> PP.optIws

    col <- P.getColumn

    result <- P.localPeek >>= case _ of
      Nothing -> pure Nothing
      Just _ -> Just <$> P.absolute (parseExpression context)

    PP.eatEol >>=
      if _ then ado
        -- In case the amount of whitespace has not yet been set,
        -- we set it here, but only if we parsed some expressions on this line.
        tail <- loop $ ws <|> (result $> col)
        in fromMaybe Empty result : tail
      else
        pure $ List.fromFoldable result

-- }}}
-- {{{ Toplevel statemenets 
parseFor :: P.Parser Toplevel
parseFor = do
  PP.string "for"
  name <- PP.reqIws *> parseVariableName
  PP.reqIws *> PP.string "<-"
  value <- PP.reqIws *> parseExpression None
  pure $ For name value

parseSnippetBody :: Trigger -> P.Parser Snippet
parseSnippetBody trigger = P.label "snippet body" do
  commands <- PP.many $ PP.optWs *> P.absolute snipCommand
  liftEither $ makeSnippet trigger commands
  where
  snipCommand :: P.Parser (Maybe SnippetCommand)
  snipCommand = P.localPeek >>= case _ of
    Just "n" -> P.label "snippet name" do
      PP.string "name"
      name <- PP.reqIws *> parseExpression None
      pure $ Just $ SetName name
    Just "d" -> P.label "snippet description" do
      PP.string "desc"
      desc <- PP.reqIws *> parseExpression None
      pure $ Just $ SetDescription desc
    Just "s" -> P.label "snippet expansion" do
      PP.string "snip"
      expansion <- PP.optIws *> P.agt parseChildExpression
      pure $ Just $ SetExpansion expansion
    _ -> pure Nothing

parseStringSnippet :: P.Parser Toplevel
parseStringSnippet = do
  triggerKind <- P.label "snippet trigger kind" do
    P.localPeek >>= case _ of
      Just "s" -> PP.string "string" $> String
      Just "p" -> PP.string "pattern" $> Pattern
      _ -> P.fail "invalid trigger"

  triggerExpr <- P.label "snippet trigger" $ PP.reqIws *> parseExpression None
  snip <- P.agt $ parseSnippetBody $ triggerKind /\ triggerExpr
  pure $ Snippet snip

parseBlock :: P.Parser Toplevel
parseBlock = do
  PP.string "block"
  modifiers <- PP.many $ PP.optIws *> modifier
  elements <- P.agt parseBlockElements
  let partitions = Array.partition fst $ Array.fromFoldable modifiers
  pure $ Block
    { elements
    , modifiers:
        { positive: partitions.yes <#> snd
        , negative: partitions.no <#> snd
        }
    }
  where
  modifier = P.label "modifier" do
    PP.eatEol >>=
      if _ then pure Nothing
      else
        P.token $ Just <$> do
          P.peek >>= case _ of
            Just "!" -> do
              void P.next
              name <- modifierName
              pure $ false /\ name
            _ -> do
              name <- modifierName
              pure $ true /\ name

  modifierName =
    PP.takeWhile1 "block modifier name" $ PP.regex "[a-zA-Z0-9]"

parseAbbreviation :: P.Parser Toplevel
parseAbbreviation = P.label "abbreviation" do
  PP.string "abbr"
  trigger <- P.label "abbreviation trigger" $ PP.reqIws *> parseExpression Inline
  expansion <- P.label "abbreviation expansion" $ PP.reqIws *> parseExpression None
  pure $ Snippet
    { description: Nothing
    , name: Nothing
    , expansion
    , trigger
    , triggerKind: String
    }

parseBlockElement :: P.Parser (Maybe Toplevel)
parseBlockElement = P.label "block element" do
  P.localPeek >>= case _ of
    Just "f" -> Just <$> parseFor
    Just "b" -> Just <$> parseBlock
    Just "s" -> Just <$> parseStringSnippet
    Just "p" -> Just <$> parseStringSnippet
    Just "a" -> Just <$> parseAbbreviation
    _ -> pure Nothing

parseBlockElements :: P.Parser Scope
parseBlockElements = defer \_ -> P.label "block elements" do
  elements <- PP.many (PP.optWs *> P.absolute parseBlockElement)
  pure $ Array.fromFoldable elements

parseToplevel :: P.Parser Scope
parseToplevel = P.label "toplevel" do
  P.withRelation (P.IConst 1)
    $ P.absolute
    $ parseBlockElements

-- }}}
