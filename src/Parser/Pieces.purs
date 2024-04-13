-- | Contains a few premade indentation-aware parsers
module Miros.Parser.Pieces
  ( takeWhile
  , takeWhile1
  , many
  , optWs
  , reqWs
  , optIws
  , reqIws
  , isDigit
  , isIws
  , isWs
  , exactWhitespace
  , nat
  , eof
  , isEol
  , eatEol
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
import Miros.Parser.Debug as PD

-- {{{ Single-token helpers
expect :: P.Rune -> P.Parser Unit
expect e = P.label ("expect " <> show e) do
  P.token P.next >>= case _ of
    Just c | c == e -> pure unit
    _ -> P.fail $ "Expected " <> show e

eof :: P.Parser Unit
eof = do
  P.peek >>= case _ of
    Nothing -> pure unit
    Just _ -> P.fail "Expected eof"

isEol :: P.Parser Boolean
isEol = do
  P.peek >>= case _ of
    Just "\n" -> pure true
    _ -> pure false

eatEol :: P.Parser Boolean
eatEol = do
  P.peek >>= case _ of
    Just "\n" -> P.next $> true
    _ -> pure false

-- | Like `peek`, but errors out on <eof>
forcePeek :: P.Parser P.Rune
forcePeek = P.peek >>= case _ of
  Nothing -> P.fail "unexpected oef"
  Just c -> pure c

-- }}}
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

sepBy :: forall a. P.Rune -> P.Parser a -> P.Parser (List a)
sepBy separator parser = fix \self -> do
  e <- parser
  forcePeek >>= \c ->
    if c == separator then do
      expect separator
      Cons e <$> self
    else
      pure $ pure e

-- | Runs a parser until said parser returns `Nothing`
many :: forall a. Debug a => P.Parser (Maybe a) -> P.Parser (List a)
many chunk = P.label "many" $ fix \self -> PD.do
  first <- chunk
  case first of
    Just head -> Cons head <$> self
    Nothing -> pure Nil

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

exactWhitespace :: Int -> P.Parser Unit
exactWhitespace col = P.label (show (col - 1) <> " spaces") do
  go $ col - 1
  where
  go i = P.peek >>= case _ of
    Just c | i > 0 && isIws c -> P.next *> go (i - 1)
    _ -> pure unit

-- }}}
-- {{{ Numbers
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
digit = P.label "digit" do
  P.next >>= case _ of
    Just c | isDigit c, Just d <- Int.fromString c -> pure d
    _ -> P.fail "Exepected digit"

nat :: P.Parser Int
nat = P.label "natural" do
  optIws *> P.token do
    string <- takeWhile1 "digit" isDigit
    case Int.fromString string of
      Just num -> pure num
      Nothing -> P.fail $ "Invalid natural " <> string

-- }}}

regex :: String -> String -> Boolean
regex input = Regex.test $ unsafePartial $ fromJust $ hush $ Regex.regex input mempty

literal :: P.Parser String
literal = optIws *> (P.token $ takeWhile1 "literal" $ not isWs)

