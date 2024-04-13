module Miros.Parser.Lib
  ( Parser
  , ParserState
  , Indentation
  , IndentationRange
  , IndentationRelation(..)
  , Rune
  , getColumn
  , getLine
  , absolute
  , withRelation
  , agt
  , agte
  , token
  , indented
  , runParser
  , peek
  , next
  , peekMany
  , nextMany
  , localPeek
  , fail
  , label
  , log
  ) where

import Miros.Prelude
import Data.Array as Array

-- {{{ Basic types
type Indentation = Int
type IndentationRange = Indentation /\ Indentation
data IndentationRelation
  = IEq
  | IGt
  | IGte
  | IAny
  | IConst Indentation

type ParserState =
  { indentationRange :: IndentationRange
  , absolute :: Boolean
  , relation :: IndentationRelation
  }

type Location = Int /\ Int

type ParserError =
  { message :: String
  , location :: Location
  , context :: Array String
  }

-- Full unicode char
type Rune = String

derive instance Generic IndentationRelation _
instance Debug IndentationRelation where
  debug = genericDebug

-- }}}
-- {{{ FFI 
foreign import data Parser :: Type -> Type
foreign import bindImpl :: forall a b. Parser a -> (a -> Parser b) -> Parser b
foreign import pureImpl :: forall a. a -> Parser a
foreign import mapImpl :: forall a b. (a -> b) -> Parser a -> Parser b
foreign import deferImpl :: forall a. (Unit -> Parser a) -> Parser a
foreign import nextMany :: Int -> Parser (Array Rune)
foreign import peekMany :: Int -> Parser (Array Rune)
foreign import getState :: Parser ParserState
foreign import setState :: ParserState -> Parser Unit
foreign import fail :: forall a. String -> Parser a
foreign import labelImpl :: forall a. String -> Parser a -> Parser a
foreign import log :: String -> Parser Unit
foreign import runParserImpl
  :: forall a r
   . (forall x y. x -> y -> x /\ y)
  -> (a -> r)
  -> (ParserError -> r)
  -> Boolean
  -> ParserState
  -> String
  -> Parser a
  -> r

foreign import getLine :: Parser Int
foreign import getColumn :: Parser Int

instance Functor Parser where
  map = mapImpl

instance Apply Parser where
  apply = ap

instance Applicative Parser where
  pure = pureImpl

instance Bind Parser where
  bind = bindImpl

instance Monad Parser

instance MonadState ParserState Parser where
  state f = do
    old <- getState
    let result /\ new = f old
    setState new
    pure result

instance Lazy (Parser a) where
  defer = deferImpl

-- }}}
-- {{{ Basic pure functions 
inRange :: IndentationRange -> Indentation -> Boolean
inRange (from /\ to) i = from <= i && i <= to

intersectRanges :: IndentationRange -> IndentationRange -> IndentationRange
intersectRanges (afrom /\ ato) (bfrom /\ bto) = max afrom bfrom /\ min ato bto

applyRelation :: IndentationRelation -> IndentationRange -> IndentationRange
applyRelation IEq (from /\ to) = from /\ to
applyRelation IGt (from /\ to) = (from + 1) /\ top
applyRelation IGte (from /\ to) = from /\ top
applyRelation IAny _ = bottom /\ top
applyRelation (IConst i) _ = i /\ i

unapplyRelation
  :: IndentationRelation
  -> IndentationRange
  -> IndentationRange
  -> IndentationRange
unapplyRelation
  relation
  initial@(ifrom /\ ito)
  final@(ffrom /\ fto) = case relation of
  IEq -> intersectRanges initial final
  IGt -> intersectRanges initial (bottom /\ (fto - 1))
  IGte -> intersectRanges initial (bottom /\ fto)
  IConst i -> initial
  IAny -> initial

relationSymbol :: IndentationRelation -> String
relationSymbol = case _ of
  IGte -> "≥"
  IGt -> ">"
  IEq -> "="
  IAny -> "⊤"
  IConst i -> "= " <> show i

-- }}}

peek :: Parser (Maybe Rune)
peek = map Array.head (peekMany 1)

next :: Parser (Maybe Rune)
next = map Array.head (nextMany 1)

label :: forall a. String -> Parser a -> Parser a
label text = labelImpl $ "+ " <> text

absolute :: forall a. Parser a -> Parser a
absolute p = do
  modify_ _ { absolute = true }
  result <- labelImpl "> absolute" p
  modify_ _ { absolute = false }
  pure result

agt :: forall a. Parser a -> Parser a
agt = absolute >>> withRelation IGt

agte :: forall a. Parser a -> Parser a
agte = absolute >>> withRelation IGte

withRelation :: forall a. IndentationRelation -> Parser a -> Parser a
withRelation relation parser = do
  initial <- get
  if initial.absolute then parser
  else do
    result <- labelImpl ("> apply (" <> relationSymbol relation <> ")") do
      let
        initialRange = applyRelation
          relation
          initial.indentationRange

      log $ "Indentation " <> pretty initialRange
      put $ initial
        { relation = relation
        , indentationRange = initialRange
        }
      parser

    state <- get

    let
      finalRange =
        unapplyRelation
          relation
          initial.indentationRange
          state.indentationRange

    log $ "Indentation " <> pretty finalRange
    put $ state
      { relation = initial.relation
      , indentationRange = finalRange
      }

    pure result

token :: forall a. Debug a => Parser a -> Parser a
token = withRelation IGt <<< indented

-- | Lift a normal parser into one which traces indentation. 
indented :: forall a. Debug a => Parser a -> Parser a
indented parser = do
  column <- getColumn
  result <- parser
  state <- get

  unless (inRange state.indentationRange column) do
    void $ fail $ Array.fold
      [ "Invalid indentation "
      , show column
      , " (expected a value in the range "
      , show state.indentationRange
      , "), while parsing value "
      , pretty result
      ]

  put $ state
    { absolute = false
    , indentationRange = column /\ column
    }

  log $ "Indentation " <> pretty column

  pure result

-- | Similar to `peek`, except returns `Nothing` if the next token would violate
-- | the local indentation rules.
localPeek :: Parser (Maybe Rune)
localPeek = do
  column <- getColumn
  state <- get

  if inRange state.indentationRange column then
    peek
  else
    pure Nothing

runParser
  :: forall a
   . String
  -> Parser a
  -> Either ParserError a
runParser = runParserImpl (/\) Right Left false
  { indentationRange: bottom /\ top
  , absolute: false
  , relation: IAny
  }
