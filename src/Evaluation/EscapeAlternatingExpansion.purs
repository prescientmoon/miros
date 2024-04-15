module Miros.Evaluation.EscapeAlternatingExpansion
  ( EscapeAlternatingExpansion
  , unescaped
  , escaped
  , singleton
  , collapse
  , asUnescaped
  ) where

import Miros.Prelude

import Data.Array as Array
import Miros.Evaluation.Expansion as E

-- | (1) A sequence of expansions where the indices matching a given partiy are 
-- | considered "escaped".
-- | (2) No elements of the underlying array can be empty.
data EscapeAlternatingExpansion = Make Parity (Array E.Expansion)

derive instance Eq EscapeAlternatingExpansion
instance Semigroup EscapeAlternatingExpansion where
  append left@(Make parityLeft arrLeft) right@(Make parityRight arrRight) = do
    -- The parity of the last element of the first array. That is, the parity
    -- that satisfies `Make ? [last]` without breaking any invariants.
    --
    -- Intuitively, we need to traverse (length - 1) element boundaries to get
    -- from the first to the last element of the first array. At each element
    -- boundary, the parity gets swapped. Moreover, swap^n = (+) (parity n),
    -- which can be easily proven by induction:
    -- - For n=0 we clearly have `swap^n = id = (+) Even`
    -- - For n=k+1 we have 
    --    swap^n = swap^k ∘ swap
    --           = ((+) (parity k)) ∘ swap
    --           = ((+) (parity k)) ∘ ((+) Odd)
    --           = ((+) (parity k)) ∘ ((+) (parity 1))
    --           = (+) (parity (k + 1))
    let lastLeftParity = parityLeft + parity (Array.length arrLeft - 1)
    if lastLeftParity == parityRight then
      case Array.unsnoc arrLeft, Array.uncons arrRight of
        Just { init, last }, Just { head, tail } ->
          Make parityLeft $ init <> [ last <> head ] <> tail
        Just _, Nothing -> left
        Nothing, _ -> right
    else
      Make parityLeft $ arrLeft <> arrRight

instance Monoid EscapeAlternatingExpansion where
  mempty = unescaped mempty

unescaped :: E.Expansion -> EscapeAlternatingExpansion
unescaped = Make Even <<< pure

escaped :: E.Expansion -> EscapeAlternatingExpansion
escaped = Make Odd <<< pure

singleton :: E.ExpansionChunk -> EscapeAlternatingExpansion
singleton = unescaped <<< E.singleton

-- | Maps every unescaped sequence, keeping the escaped sequences intact.
collapse
  :: (E.Expansion -> E.ExpansionChunk)
  -> EscapeAlternatingExpansion
  -> E.Expansion
collapse handleUnescaped (Make escapedParity expansion) =
  expansion
    # Array.mapWithIndex
        ( \i expansion ->
            if escapedParity /= parity i then expansion
            else E.singleton $ handleUnescaped expansion
        )
    # fold

-- | Attempt to treat the entire sequence as containing only unescaped sequences.
asUnescaped :: EscapeAlternatingExpansion -> Maybe E.Expansion
asUnescaped (Make Odd _) = Nothing
asUnescaped (Make Even arr) =
  if Array.length arr > 1 then Nothing
  else Array.head arr
