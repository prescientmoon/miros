module Main where

import Miros.Prelude

import Miros.Parser.Lib (runParser)
import Miros.Parser.Implementation (parser)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  file <- readTextFile UTF8 "./input"
  case runParser parser file of
    Left err -> logPretty err
    Right result -> do
      logPretty result
