module Miros.Parser.Implementation
  ( Name
  , Block
  , Index
  , Trigger(..)
  , Snippet
  , Expr(..)
  , Toplevel
  , parser
  , expr
  ) where

import Miros.Prelude

import Data.Array as Array
import Data.List as List
import Miros.Parser.Lib as P
import Miros.Parser.Pieces as PP

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

data Toplevel
  = Block (Array Toplevel)
  | For Name Expr
  | Snippet Snippet

parseNonempty :: P.Parser Expr
parseNonempty = do
  PP.expect "$"
  PP.expect "?"
  index <- PP.digit
  PP.expect "{"
  nested <- expr false
  PP.expect "}"
  pure $ Nonempty index nested

parseChoice :: P.Parser Expr
parseChoice = do
  PP.expect "$"
  PP.expect "|"
  index <- PP.digit
  PP.expect "{"
  children <- PP.forcePeek >>= case _ of
    "\n" -> do
      void P.next
      P.agt $ PP.sepBy "," $ P.absolute $ PP.optWs *> expr true
    _ -> P.agt $ PP.sepBy "," $ expr false
  P.agte $ PP.expect "}"
  pure $ Choice index $ Array.fromFoldable children

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

parseEscape :: P.Parser Expr
parseEscape = do
  PP.expect "@"
  PP.expect "^"
  PP.expect "{"
  nested <- PP.forcePeek >>= case _ of
    "\n" -> do
      P.next *> PP.optWs
      P.agt $ expr true
    _ -> expr false
  P.agte $ PP.expect "}"
  pure $ Escape nested

parseArray :: P.Parser Expr
parseArray = do
  PP.expect "@"
  PP.expect "{"
  head <- expr false
  contents <- P.peek >>= \c -> case head, c of
    Var name, Just ":" -> do
      void P.next
      children <- PP.sepBy "," $ expr false
      pure $ ArrayIndex name $ Array.fromFoldable children
    _, Just "," -> do
      void P.next
      tail <- PP.sepBy "," $ expr false
      pure $ Array $ Array.fromFoldable $ head : tail
    _, _ -> pure $ Array [ head ]
  PP.expect "}"
  pure contents

expr :: Boolean -> P.Parser Expr
expr multiline = many <#> Array.fromFoldable >>> case _ of
  [ one ] -> one
  lots -> Concat lots
  where
  many = fix \self -> do
    first <- atom
    case first of
      Just head -> Cons head <$> self
      Nothing -> pure Nil

  lit l = P.token (P.next *> pure (Just $ Literal l))
  commit parser = Just <$> parser

  atom = P.peekMany 2 >>= List.fromFoldable >>> case _ of
    "$" : "?" : _ -> commit parseNonempty
    "$" : "|" : _ -> commit parseChoice
    "$" : _ -> commit parseTabstop
    "@" : "{" : _ -> commit parseArray
    "@" : "^" : _ -> commit parseEscape
    "@" : n : _
      | PP.isDigit n -> commit parseCaptureGroupRef
      | otherwise -> commit parseVariable
    "\\" : "," : _ -> lit ","
    "\\" : ":" : _ -> lit ":"
    "\\" : "}" : _ -> lit "}"
    "\\" : "@" : _ -> lit "@"
    "\\" : "$" : _ -> lit "$"
    "\\" : "\\" : _ -> lit "\\"
    "}" : _ -> pure Nothing
    "," : _ -> pure Nothing
    ":" : _ -> pure Nothing
    Nil -> pure Nothing
    "\n" : _
      | multiline -> lit "\n"
      | otherwise -> pure Nothing
    _ -> commit $ P.token do
      s <- PP.takeWhile $ flip Array.notElem [ "\\", ",", "@", "}", "$", ":" ]
      pure $ Literal s

parser :: P.Parser Expr
parser = P.absolute $ expr true

derive instance Generic Expr _
instance Debug Expr where
  debug e = genericDebug e
