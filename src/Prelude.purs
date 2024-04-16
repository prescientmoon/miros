module Miros.Prelude
  ( module Prelude
  , module Data.Maybe
  , module Data.Tuple
  , module Data.Tuple.Nested
  , module Control.Monad.State
  , module Data.List
  , module Data.Either
  , module Data.Foldable
  , module Effect
  , module Effect.Class
  , module Effect.Class.Console
  , module Effect.Aff
  , module Effect.Exception.Unsafe
  , module Data.Debug
  , module Data.Generic.Rep
  , module Debug
  , module Control.Alternative
  , module Control.Lazy
  , module Partial.Unsafe
  , module Control.Monad.Error.Class
  , module Data.HashMap
  , module Data.HashSet
  , module Data.FoldableWithIndex
  , module Data.Traversable
  , module Data.Array.NonEmpty
  , module Data.Int
  , module Control.Monad.Except
  , module Data.Newtype
  , module Data.String
  , unimplemented
  , logPretty
  , traceMPretty
  , unlines
  , pretty
  , throwawayState
  , indentString
  ) where

import Prelude

import Control.Alternative (class Alt, class Alternative, empty, (<|>))
import Control.Lazy (class Lazy, defer, fix)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, catchJust, liftEither, liftMaybe, throwError, try, withResource)
import Control.Monad.Except (class MonadError, class MonadTrans, Except, ExceptT(..), catchError, catchJust, except, lift, mapExcept, mapExceptT, runExcept, runExceptT, throwError, withExcept, withExceptT)
import Control.Monad.State (class MonadState, class MonadTrans, State, StateT(..), evalState, evalStateT, execState, execStateT, get, gets, lift, mapState, mapStateT, modify, modify_, put, runState, runStateT, state, withState, withStateT)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Debug (class Debug, class DebugRowList, class GenericDebug, class GenericDebugArgs, DiffOptions, PrettyPrintOptions, Repr, ReprDelta, array, assoc, boolean, char, collection, constructor, debug, debugRowList, defaultDiffOptions, defaultPrettyPrintOptions, diff, diffRepr, diffReprWith, genericDebug, genericDebug', genericDebugArgs, opaque, opaqueLiteral, opaque_, prettyPrint, prettyPrintDelta, prettyPrintDeltaWith, prettyPrintWith, record, string)
import Data.Either (Either(..), blush, either, fromLeft, fromLeft', fromRight, fromRight', hush, isLeft, isRight, note, note')
import Data.Foldable (class Foldable, all, and, any, elem, find, findMap, fold, foldM, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for_, indexl, indexr, intercalate, length, lookup, maximum, maximumBy, minimum, minimumBy, notElem, null, oneOf, oneOfMap, or, product, sequence_, sum, surround, surroundMap, traverse_)
import Data.FoldableWithIndex (class FoldableWithIndex, allWithIndex, anyWithIndex, findMapWithIndex, findWithIndex, foldMapDefault, foldMapWithIndex, foldMapWithIndexDefaultL, foldMapWithIndexDefaultR, foldWithIndexM, foldlWithIndex, foldlWithIndexDefault, foldrWithIndex, foldrWithIndexDefault, forWithIndex_, surroundMapWithIndex, traverseWithIndex_)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from, repOf, to)
import Data.HashMap (HashMap, SemigroupHashMap(..))
import Data.HashSet (HashSet)
import Data.Int (Parity(..), Radix, base36, binary, decimal, even, hexadecimal, octal, odd, parity, radix)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional)
import Data.Newtype (class Newtype, ala, alaF, collect, over, over2, overF, overF2, un, under, under2, underF, underF2, unwrap, wrap)
import Data.String (CodePoint, Pattern(..), Replacement(..), joinWith)
import Data.String as String
import Data.Traversable (class Foldable, class Traversable, Accum, all, and, any, elem, find, fold, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for, for_, intercalate, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, oneOf, or, scanl, scanr, sequence, sequenceDefault, sequence_, sum, traverse, traverseDefault, traverse_)
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry)
import Data.Tuple.Nested (type (/\), T10, T11, T2, T3, T4, T5, T6, T7, T8, T9, Tuple1, Tuple10, Tuple2, Tuple3, Tuple4, Tuple5, Tuple6, Tuple7, Tuple8, Tuple9, curry1, curry10, curry2, curry3, curry4, curry5, curry6, curry7, curry8, curry9, get1, get10, get2, get3, get4, get5, get6, get7, get8, get9, tuple1, tuple10, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8, tuple9, uncurry1, uncurry10, uncurry2, uncurry3, uncurry4, uncurry5, uncurry6, uncurry7, uncurry8, uncurry9, (/\))
import Debug (traceM)
import Effect (Effect, forE, foreachE, untilE, whileE)
import Effect.Aff (Aff, BracketConditions, Canceler(..), Error, Fiber, Milliseconds(..), ParAff, apathize, attempt, bracket, cancelWith, catchError, delay, effectCanceler, error, fiberCanceler, finally, forkAff, generalBracket, invincible, joinFiber, killFiber, launchAff, launchAff_, launchSuspendedAff, makeAff, message, never, nonCanceler, parallel, runAff, runAff_, runSuspendedAff, sequential, supervise, suspendAff, throwError, try)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (clear, group, groupCollapsed, groupEnd, grouped, infoShow, log, logShow, time, timeEnd, timeLog, warn, warnShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafePartial)
import Prim.TypeError (class Warn, Text)

unimplemented :: forall a. Warn (Text "unimplemenet") => a
unimplemented = unsafeThrow "unimplemented"

pretty :: forall @a. Debug a => a -> String
pretty a =
  prettyPrintWith
    defaultPrettyPrintOptions { maxDepth = Nothing }
    $ debug a

logPretty :: forall m @a. Debug a => MonadEffect m => a -> m Unit
logPretty a = log $ pretty a

traceMPretty :: forall m @a. Debug a => Monad m => a -> m Unit
traceMPretty a = traceM $ pretty a

unlines :: Array String -> String
unlines = joinWith "\n"

-- | Run a computation, discarding any changes it makes to the state.
throwawayState :: forall m s. MonadState s m => m ~> m
throwawayState f = do
  s <- get
  result <- f
  put s
  pure result

indentString :: Int -> String -> String
indentString i =
  String.split (String.Pattern "\n")
    >>> map ((<>) (Array.fold $ Array.replicate i " "))
    >>> unlines
