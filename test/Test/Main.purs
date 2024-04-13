module Test.Main where

import Miros.Prelude

import Miros.Parser.Implementation (parseToplevel, parseMultilineExpression)
import Miros.Parser.Lib (Parser, runParser)
import Miros.Parser.Pieces (eof, optWs)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Test.Spec (Spec, describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

shouldAllParse :: forall a. Debug a => String -> Parser a -> Aff (Spec Unit)
shouldAllParse path parser = do
  contents <- readdir path
  pure $ describe "successes" do
    for_ contents \filename -> do
      it ("should parse " <> filename) do
        file <- readTextFile UTF8 $ path <> "/" <> filename
        case runParser file (parser <* optWs <* eof) of
          Left err -> throwError $ error $ pretty err
          Right _ -> pure unit

noneShouldParse :: forall a. Debug a => String -> Parser a -> Aff (Spec Unit)
noneShouldParse path parser = do
  contents <- readdir path
  pure $ describe "failures" do
    for_ contents \filename -> do
      it ("should fail on " <> filename) do
        file <- readTextFile UTF8 $ path <> "/" <> filename
        case runParser file (parser <* optWs <* eof) of
          Left _ -> pure unit
          Right result -> throwError
            $ error
            $ "parsing succeeded with "
            <> pretty result

main :: Effect Unit
main = launchAff_ do
  shouldParseExpressions <- shouldAllParse "./test/inputs/expr/good" parseMultilineExpression
  shouldntParseExpressions <- noneShouldParse "./test/inputs/expr/bad" parseMultilineExpression
  shouldParseToplevel <- shouldAllParse "./test/inputs/toplevel/good" parseToplevel
  shouldntParseToplevel <- noneShouldParse "./test/inputs/toplevel/bad" parseToplevel
  runSpec [ consoleReporter ] do
    describe "parser" do
      describe "expressions" do
        shouldParseExpressions
        shouldntParseExpressions
      describe "toplevel" do
        shouldParseToplevel
        shouldntParseToplevel

