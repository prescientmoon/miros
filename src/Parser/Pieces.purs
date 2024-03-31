-- | Contains a few premade indentation-aware parsers
module Miros.Parser.Pieces
  ( nl
  , optWs
  , reqWs
  , optIws
  , reqIws
  , nat
  , many
  , many1
  , eof
  , try
  ) where

import Miros.Prelude

import Data.Int as Int
import Data.List as List
import Miros.Parser.Lib as P
import StringParser as SP

nl :: SP.Parser Unit
nl = void $ flip SP.withError "failed to match single newline" $ SP.regex "\n"

optIws :: P.Parser Unit
optIws = P.liftUnchecked $ void $ flip SP.withError "failed to match one or more inline whitespace" $ SP.regex "[ \t]+"

reqIws :: P.Parser Unit
reqIws = P.liftUnchecked $ void $ flip SP.withError "failed to match zero or more inline whitespace" $ SP.regex "[ \t]*"

optWs :: P.Parser Unit
optWs = P.liftUnchecked $ void $ flip SP.withError "failed to match one or more whitespace" $ SP.regex "[ \n\t]+"

reqWs :: P.Parser Unit
reqWs = P.liftUnchecked $ void $ flip SP.withError "failed to match zero or more whitespace" $ SP.regex "[ \n\t]*"

nat :: P.Parser Int
nat = P.indented do
  string <- flip SP.withError "failed to match natural" $ SP.regex "[0-9]+"
  case Int.fromString string of
    Just num -> pure num
    Nothing -> SP.fail $ "Invalid natural " <> string

-- | Run given parser and fail if the parser did not consume any input.
assertConsume :: forall a. P.Parser a -> P.Parser a
assertConsume (StateT p) = StateT \s ->
  SP.assertConsume $ p s

-- | Match a parser zero or more times.
-- | Stops matching when the parser fails or does not consume anymore.
many :: forall a. P.Parser a -> P.Parser (List a)
many = List.manyRec <<< assertConsume

-- | Match a parser one or more times.
-- | Stops matching when the parser fails or does not consume anymore.
many1 :: forall a. P.Parser a -> P.Parser (List a)
many1 p = Cons <$> p <*> many p

-- | `try p` means: run `p` but do not consume input in case of failure.
try :: forall a. P.Parser a -> P.Parser a
try (StateT p) = StateT \s -> SP.try (p s)

eof :: P.Parser Unit
eof = P.liftUnchecked SP.eof
