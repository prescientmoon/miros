module Test.Main where

import Miros.Prelude

import Miros.Parser.Implementation (multilineExpr)
import Miros.Parser.Lib (runParser)
import Miros.Parser.Pieces (eof)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Test.Spec (Spec, describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

shouldAllParse :: String -> Aff (Spec Unit)
shouldAllParse path = do
  contents <- readdir path
  pure $ describe "successes" do
    for_ contents \filename -> do
      it ("should parse " <> filename) do
        file <- readTextFile UTF8 $ path <> "/" <> filename
        case runParser file (multilineExpr <* eof) of
          Left err -> throwError $ error $ pretty err
          Right result -> pure unit

noneShouldParse :: String -> Aff (Spec Unit)
noneShouldParse path = do
  contents <- readdir path
  pure $ describe "failures" do
    for_ contents \filename -> do
      it ("should fail on " <> filename) do
        file <- readTextFile UTF8 $ path <> "/" <> filename
        case runParser file (multilineExpr <* eof) of
          Left err -> pure unit
          Right result -> throwError
            $ error
            $ "parsing succeeded with "
            <> pretty result

main :: Effect Unit
main = launchAff_ do
  shouldParseExpressions <- shouldAllParse "./test/inputs/expr/good"
  shouldntParseExpressions <- noneShouldParse "./test/inputs/expr/bad"
  runSpec [ consoleReporter ] do
    describe "parser" do
      describe "expressions" do
        shouldParseExpressions
        shouldntParseExpressions

