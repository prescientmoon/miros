module Miros.Parser.Lib
  ( IndentationRelation(..)
  , IndentationRange
  , Indentation
  , Column
  , ParserState
  , Parser
  , indented
  , withRelation
  , absolute
  , liftUnchecked
  , runParser
  ) where

import Miros.Prelude

import Data.Array as Array
import Data.String.CodeUnits as CU
import StringParser as SP

type Indentation = Int
type Column = Int
type IndentationRange = Indentation /\ Indentation
data IndentationRelation
  = IEq
  | IGt
  | IGte
  | IAny
  | IConst Indentation

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

type ParserState =
  { indentationRange :: IndentationRange
  , column :: Column
  , absolute :: Boolean
  , relation :: IndentationRelation
  }

type Parser a = StateT ParserState SP.Parser a

absolute :: forall a. Parser a -> Parser a
absolute p = modify_ _ { absolute = true } *> p

withRelation :: forall a. IndentationRelation -> Parser a -> Parser a
withRelation relation parser = do
  initial <- get
  if initial.absolute then parser
  else do
    put $ initial
      { relation = relation
      , indentationRange =
          applyRelation
            relation
            initial.indentationRange
      }
    traceM $ fold
      [ "Apply "
      , pretty relation
      , ": "
      , pretty $ applyRelation relation initial.indentationRange
      ]
    result <- parser
    state <- get
    put $ state
      { relation = initial.relation
      , indentationRange =
          unapplyRelation
            relation
            initial.indentationRange
            state.indentationRange
      }
    traceM $ fold
      [ "Unapply "
      , pretty relation
      , ": "
      , pretty $ unapplyRelation
          initial.relation
          initial.indentationRange
          state.indentationRange
      ]
    pure result

currentSubstring :: Parser SP.PosString
currentSubstring = lift $ SP.Parser \string -> pure
  { result: string
  , suffix: string
  }

-- | Traverse a string, and compute the final column position
updateColumn
  :: String
  -> Int
  -> Column
  -> Column
updateColumn string len = go 0
  where
  go at indentation
    | at == len = indentation
    | otherwise = go (at + 1)
        case CU.charAt at string of
          Just '\n' -> 0
          _ -> indentation + 1

-- | Lift a normal parser into one which traces indentation. 
-- |
-- | Accepts a flag which specifies whether to ensure the indentation is correct. 
-- |
-- | This is typically set to false for space-parsers only.
liftParser :: forall a. Debug a => Boolean -> SP.Parser a -> Parser a
liftParser nonSpace parser = do
  start <- currentSubstring
  result <- lift parser
  end <- currentSubstring
  state <- get

  let
    column = updateColumn
      start.substring
      (end.position - start.position)
      state.column

  traceM $ "Parsed: " <> pretty result
  traceM $ "Remaining: " <> show end.substring
  traceM $ "Column: " <> pretty column

  let outOfRange = not $ inRange state.indentationRange state.column
  when (nonSpace && outOfRange) $ do
    traceM "Indentation failure"
    lift $ SP.fail $ Array.fold
      [ "Invalid indentation "
      , show column
      , " (expected a value in the range "
      , show state.indentationRange
      , "), while parsing value "
      , pretty result
      ]

  if nonSpace then do
    put $ state
      { column = column
      , absolute = false
      , indentationRange = state.column /\ state.column
      }
    traceM $ fold
      [ "Indentation: "
      , pretty state.indentationRange
      , " => "
      , pretty $ state.column /\ state.column
      ]
    traceM $ fold
      [ "Absolute: "
      , pretty state.absolute
      , " => "
      , pretty false
      ]

  else do
    traceM $ "Indentation: " <> pretty state.indentationRange
    put $ state { column = column }

  traceM ""

  pure result

-- | Lift a normal parser into one which traces indentation
indented :: forall a. Debug a => SP.Parser a -> Parser a
indented = liftParser true

-- | Lift a whitespace parser into one which traces indentation
liftUnchecked :: forall a. Debug a => SP.Parser a -> Parser a
liftUnchecked = liftParser false

runParser :: forall a. Parser a -> String -> Either SP.ParseError a
runParser p input =
  flip
    SP.runParser
    input $
    evalStateT p
      { indentationRange: bottom /\ top
      , column: 0
      , absolute: false
      , relation: IAny
      }

derive instance Generic IndentationRelation _
instance Debug IndentationRelation where
  debug = genericDebug
