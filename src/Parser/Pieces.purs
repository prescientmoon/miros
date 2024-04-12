-- | Contains a few premade indentation-aware parsers
module Miros.Parser.Pieces
  ( takeWhile
  , takeWhile1
  , optWs
  , reqWs
  , optIws
  , reqIws
  , isDigit
  , nat
  , eof
  , literal
  , forcePeek
  , digit
  , expect
  , sepBy
  , regex
  ) where

import Miros.Prelude

import Data.Int as Int
import Data.String.Regex as Regex
import Miros.Parser.Lib as P

-- {{{ Repetition hepers
takeWhile :: (P.Rune -> Boolean) -> P.Parser String
takeWhile predicate = go ""
  where
  go acc = P.peek >>= case _ of
    Just c | predicate c -> P.next *> go (acc <> c)
    _ -> pure acc

takeWhile1 :: String -> (P.Rune -> Boolean) -> P.Parser String
takeWhile1 thing predicate = takeWhile predicate >>= case _ of
  "" -> P.fail $ "Expected " <> thing
  s -> pure s

-- }}}
-- {{{ Whitespace helpers 
isIws :: P.Rune -> Boolean
isIws c = c == " " || c == "\t"

isWs :: P.Rune -> Boolean
isWs c = c == " " || c == "\t" || c == "\n"

optIws :: P.Parser Unit
optIws = void $ takeWhile isIws

reqIws :: P.Parser Unit
reqIws = void $ takeWhile1 "whitespace" isIws

optWs :: P.Parser Unit
optWs = void $ takeWhile isWs

reqWs :: P.Parser Unit
reqWs = void $ takeWhile1 "whitespace or newline" isWs

-- }}}

isDigit :: P.Rune -> Boolean
isDigit c = or
  -- This is painful...
  [ c == "0"
  , c == "1"
  , c == "2"
  , c == "3"
  , c == "4"
  , c == "5"
  , c == "6"
  , c == "7"
  , c == "8"
  , c == "9"
  ]

digit :: P.Parser Int
digit = P.next >>= case _ of
  Just c | isDigit c, Just d <- Int.fromString c -> pure d
  _ -> P.fail "Exepected digit"

nat :: P.Parser Int
nat = optIws *> P.token do
  string <- takeWhile1 "digit" isDigit
  case Int.fromString string of
    Just num -> pure num
    Nothing -> P.fail $ "Invalid natural " <> string

regex :: String -> String -> Boolean
regex input = Regex.test $ unsafePartial $ fromJust $ hush $ Regex.regex input mempty

literal :: P.Parser String
literal = optIws *> (P.token $ takeWhile1 "literal" $ not isWs)

expect :: P.Rune -> P.Parser Unit
expect e = P.token P.next >>= case _ of
  Just c | c == e -> pure unit
  _ -> P.fail $ "Expected " <> show e

eof :: P.Parser Unit
eof = do
  P.peek >>= case _ of
    Nothing -> pure unit
    Just _ -> P.fail "Expected eof"

-- | Like `peek`, but errors out on <eof>
forcePeek :: P.Parser P.Rune
forcePeek = P.peek >>= case _ of
  Nothing -> P.fail "unexpected oef"
  Just c -> pure c

sepBy :: forall a. P.Rune -> P.Parser a -> P.Parser (List a)
sepBy separator parser = loop
  where
  loop = fix $ \go -> do
    e <- parser
    forcePeek >>= \c ->
      if c == separator then
        P.next *> (Cons e <$> go)
      else pure $ pure e
