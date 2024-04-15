module Miros.Evaluation.Expansion
  ( ExpansionChunk(..)
  , Expansion
  , chunks
  , mergeChunks
  , singleton
  , asChunk
  ) where

import Miros.Prelude

import Miros.Ast as Ast
import Data.Array as Array

data ExpansionChunk
  = Literal String
  | TabStop Ast.Index
  | CaptureGroupRef Ast.Index
  | Nonempty Ast.Index Expansion
  | Choice Ast.Index (Array Expansion)
  | Array (Array Expansion)

-- | The flattened `Ast.Concat` representation.
-- |
-- | This construct holds the invariant that `mergeChunks l r == Nothing` for
-- | all adjacent elements.
newtype Expansion = Expansion (Array ExpansionChunk)

chunks :: Expansion -> Array ExpansionChunk
chunks (Expansion c) = c

singleton :: ExpansionChunk -> Expansion
singleton = Expansion <<< pure

-- | The partial inverse of `singleton`
asChunk :: Expansion -> Maybe ExpansionChunk
asChunk (Expansion chunks) =
  if Array.length chunks > 1 then Nothing
  else Array.head chunks

-- | Cleans up the mess created by the parser (sometimes), by attempting to
-- | merge neighbouring chunks.
-- |
-- | Invariant (1): The success of the result is not affected by swaps:
-- |   isJust (mergeChunks a b) <=> isJust (mergeChunks b a)
-- |
-- | Invariant (2): Application to the side doesn't affect the success of the
-- | result:
-- |   mergeChunks a b == Just d
-- |   then isJust (mergeChunks d c) == isJust (mergeChunks b c)
-- |
-- | Putting the two invariants together yields the strong result that, when
-- | merging two `Expansion`s, we only need to attempt to merge the last element
-- | of the first with the first element of the second.
mergeChunks :: ExpansionChunk -> ExpansionChunk -> Maybe ExpansionChunk
mergeChunks = case _, _ of
  Literal a, Literal b -> Just $ Literal $ a <> b
  Nonempty i l, Nonempty j r | i == j -> Just $ Nonempty i $ l <> r
  _, _ -> Nothing

instance Semigroup Expansion where
  append (Expansion left) (Expansion right) =
    Expansion case Array.unsnoc left, Array.uncons right of
      Just { init, last }, Just { head, tail }
        | Just merged <- mergeChunks last head -> Array.fold
            [ init, [ merged ], tail ]
      _, _ -> left <> right

derive instance Eq Expansion
derive instance Eq ExpansionChunk
derive newtype instance Monoid Expansion

derive instance Generic ExpansionChunk _
instance Debug ExpansionChunk where
  debug = genericDebug

instance Debug Expansion where
  debug (Expansion chunks) = collection "Expansion" $ debug <$> chunks
