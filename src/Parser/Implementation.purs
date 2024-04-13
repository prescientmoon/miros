module Miros.Parser.Implementation
  ( Name
  , Block
  , Index
  , Trigger(..)
  , Snippet
  , Expr(..)
  , Toplevel
  , parser
  , multilineExpr
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
  = Block (Array Toplevel)
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

parseVariable :: P.Parser Expr
parseVariable = do
  PP.expect "@"
  name <- PP.takeWhile1 "variable name" $ PP.regex "[a-zA-Z0-9]"
  pure $ Var name

parseCaptureGroupRef :: P.Parser Expr
parseCaptureGroupRef = do
  PP.expect "@"
  d <- PP.digit
  pure $ CaptureGroupRef d

-- }}}
-- {{{ Single-child expressions
childExpression :: P.Parser Expr
childExpression = PP.eatEol >>=
  if _ then P.agt multilineExpr
  else expr

-- {{{ Nonempty
parseNonempty :: P.Parser Expr
parseNonempty = P.label "nonempty expression" $ P.agt do
  PP.expect "$"
  PP.expect "?"
  index <- PP.digit
  PP.expect "{"
  nested <- childExpression
  P.agte $ PP.expect "}"
  pure $ Nonempty index nested

-- }}}
-- {{{ Escape
parseEscape :: P.Parser Expr
parseEscape = P.label "escape expression" $ P.agt do
  PP.expect "@"
  PP.expect "^"
  PP.expect "{"
  nested <- childExpression
  P.agte $ PP.expect "}"
  pure $ Escape nested

-- }}}
-- }}}
-- {{{ Multi-child expresions 
-- {{{ Choice 
parseChoice :: P.Parser Expr
parseChoice = P.label "choice expression" $ P.agt PD.do
  PP.expect "$"
  PP.expect "|"
  index <- PP.digit
  PP.expect "{"
  children <- P.label "choice expression body" do
    let mkChild = P.label "choice expression child"
    PP.eatEol >>=
      if _ then
        P.agt $ PP.sepBy "," $ mkChild $ P.absolute multilineExpr
      else
        PP.sepBy "," $ mkChild expr
  P.agte $ PP.expect "}"
  pure $ Choice index $ Array.fromFoldable children

-- }}}
-- {{{ Array (indices)
parseArray :: P.Parser Expr
parseArray = do
  PP.expect "@"
  PP.expect "{"
  head <- expr
  contents <- P.peek >>= \c -> case head, c of
    Var name, Just ":" -> do
      void P.next
      children <- PP.sepBy "," expr
      pure $ ArrayIndex name $ Array.fromFoldable children
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
multilineExpr :: P.Parser Expr
multilineExpr = P.label "multiline expression" do
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

parser :: P.Parser Expr
parser = multilineExpr

derive instance Generic Expr _
instance Debug Expr where
  debug e = genericDebug e

derive instance Eq Expr
