module Main where

import Miros.Prelude

import Miros.Parser.Implementation (parseToplevel)
import Miros.Parser.Lib (runParser)
import Miros.Parser.Pieces (eof, optWs)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  file <- readTextFile UTF8 "./input"
  case runParser file (parseToplevel <* optWs <* eof) of
    Left err -> logPretty err
    Right result -> do
      logPretty result
