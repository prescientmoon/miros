module Miros.Parser.Implementation
  ( Name
  , Block
  , Index
  , Trigger(..)
  , Snippet
  , Expr(..)
  , Toplevel(..)
  , parser
  , parseToplevel
  , parseMultilineExpression
  ) where

import Miros.Prelude

import Data.Array as Array
import Data.List as List
import Miros.Helpers.Array as Miros.Array
import Miros.Parser.Debug as PD
import Miros.Parser.Lib as P
import Miros.Parser.Pieces as PP

-- {{{ Basic types 
type Block =
  { elements :: Array Toplevel
  , modifiers ::
      { positive :: Array String
      , negative :: Array String
      }
  }

type Name = String
type Index = Int

data Trigger = String String | Pattern String

type Snippet =
  { trigger :: Expr
  , expand :: Expr
  , name :: Maybe Expr
  , description :: Maybe Expr
  }

data Expr
  = Literal String
  | Var Name
  | TabStop Index
  | CaptureGroupRef Index
  | Concat (Array Expr)
  | Array (Array Expr)
  | ArrayIndex Name (Array Expr)
  | Nonempty Index Expr
  | Choice Index (Array Expr)
  | Escape Expr
  | Empty
  | Newline

data Toplevel
  = Block Block
  | For Name Expr
  | Snippet Snippet

-- }}}
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
parseVariableName = do
  PP.expect "@"
  name <- PP.takeWhile1 "variable name" $ PP.regex "[a-zA-Z0-9]"
  pure name

parseVariable :: P.Parser Expr
parseVariable = Var <$> parseVariableName

parseCaptureGroupRef :: P.Parser Expr
parseCaptureGroupRef = do
  PP.expect "@"
  d <- PP.digit
  pure $ CaptureGroupRef d

-- }}}
-- {{{ Single-child expressions
parseChildExpression :: P.Parser Expr
parseChildExpression = PP.eatEol >>=
  if _ then P.agt parseMultilineExpression
  else expr

-- {{{ Nonempty
parseNonempty :: P.Parser Expr
parseNonempty = P.label "nonempty expression" do
  PP.string "$?"
  index <- PP.digit
  PP.expect "{"
  nested <- parseChildExpression
  P.agte $ PP.expect "}"
  pure $ Nonempty index nested

-- }}}
-- {{{ Escape
parseEscape :: P.Parser Expr
parseEscape = P.label "escape expression" do
  PP.string "@^{"
  nested <- parseChildExpression
  P.agte $ PP.expect "}"
  pure $ Escape nested

-- }}}
-- }}}
-- {{{ Multi-child expresions 
parseChildExpressions :: String -> P.Parser (List Expr)
parseChildExpressions name = P.label (name <> " expression body") do
  let mkChild = P.label (name <> "choice expression child")
  PP.eatEol >>=
    if _ then
      P.agt $ PP.sepBy "," $ mkChild $ P.absolute parseMultilineExpression
    else
      PP.sepBy "," $ mkChild expr

-- {{{ Choice 
parseChoice :: P.Parser Expr
parseChoice = P.label "choice expression" $ PD.do
  PP.string "$|"
  index <- PP.digit
  PP.expect "{"
  children <- parseChildExpressions "choice"
  P.agte $ PP.expect "}"
  pure $ Choice index $ Array.fromFoldable children

-- }}}
-- {{{ Array (indices)
parseArray :: P.Parser Expr
parseArray = P.label "array expression" PD.do
  PP.string "@{"
  contents <- PP.isEol >>=
    if _ then do
      elements <- parseChildExpressions "array"
      pure $ Array $ Array.fromFoldable elements
    else do
      head <- expr
      P.peek >>= \c -> case head, c of
        Var name, Just ":" -> do
          void P.next
          elements <- parseChildExpressions "array index"
          pure $ ArrayIndex name $ Array.fromFoldable elements
        _, Just "," -> do
          void P.next
          tail <- PP.sepBy "," expr
          pure $ Array $ Array.fromFoldable $ head : tail
        _, _ -> pure $ Array [ head ]
  PP.expect "}"
  pure contents

-- }}}
-- }}}
-- {{{ Expressions
expr :: P.Parser Expr
expr = P.label "expression" do
  PP.many chunk <#> concatExpressions
  where
  lit l = commit $ P.token $ P.next $> Literal l
  commit parser = Just <$> parser

  chunk = P.label "expression chunk" do
    P.peekMany 2 >>= List.fromFoldable >>> case _ of
      "$" : "?" : _ -> commit parseNonempty
      "$" : "|" : _ -> commit parseChoice
      "$" : _ -> commit parseTabstop
      "@" : "{" : _ -> commit parseArray
      "@" : "^" : _ -> commit parseEscape
      "@" : n : _
        | PP.isDigit n -> commit parseCaptureGroupRef
        | otherwise -> commit parseVariable
      "\\" : "," : _ -> P.next *> lit ","
      "\\" : ":" : _ -> P.next *> lit ":"
      "\\" : "}" : _ -> P.next *> lit "}"
      "\\" : "@" : _ -> P.next *> lit "@"
      "\\" : "$" : _ -> P.next *> lit "$"
      "\\" : "\\" : _ -> P.next *> lit "\\"
      "}" : _ -> pure Nothing
      "," : _ -> pure Nothing
      ":" : _ -> pure Nothing
      "\n" : _ -> pure Nothing
      Nil -> pure Nothing
      _ -> commit $ P.token do
        s <- PP.takeWhile $ flip Array.notElem [ "\\", ",", "@", "}", "$", ":", "\n" ]
        pure $ Literal s

-- }}}
-- {{{ Multline expressions
parseMultilineExpression :: P.Parser Expr
parseMultilineExpression = P.label "multiline expression" do
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
      Just _ -> Just <$> P.absolute expr

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
  value <- PP.reqIws *> expr
  pure $ For name value

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

parseBlockElement :: P.Parser (Maybe Toplevel)
parseBlockElement = P.label "block element" do
  P.localPeek >>= case _ of
    Just "f" -> Just <$> parseFor
    Just "b" -> Just <$> parseBlock
    _ -> pure Nothing

parseBlockElements :: P.Parser (Array Toplevel)
parseBlockElements = defer \_ -> P.label "block elements" do
  elements <- PP.many (PP.optWs *> P.absolute parseBlockElement)
  pure $ Array.fromFoldable elements

parseToplevel :: P.Parser (Array Toplevel)
parseToplevel = P.label "toplevel" do
  P.withRelation (P.IConst 1)
    $ P.absolute
    $ parseBlockElements

-- }}}

parser :: P.Parser Expr
parser = parseMultilineExpression

derive instance Generic Expr _
instance Debug Expr where
  debug e = genericDebug e

derive instance Eq Expr

derive instance Generic Toplevel _
instance Debug Toplevel where
  debug e = genericDebug e

derive instance Eq Toplevel
