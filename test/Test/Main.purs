module Test.Main where

import Miros.Prelude

import Data.Array as Array
import Miros.Execution (compile, resolveImport)
import Miros.Parser.Implementation (ExprContext(..), parseMultilineExpression, parseToplevel)
import Miros.Parser.Lib (Parser, runParser)
import Miros.Parser.Pieces (eof, optWs)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, stat)
import Node.FS.Stats (isFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

shouldEqual
  :: forall m t
   . MonadThrow Error m
  => Debug t
  => Eq t
  => t
  -> t
  -> m Unit
shouldEqual v1 v2 =
  when (v1 /= v2)
    $ fail
    $ prettyDelta v1 v2

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

shouldAllMatch :: String -> Aff (Spec Unit)
shouldAllMatch path = do
  subdirs <- readdir path
  pure $ describe "matches" do
    for_ subdirs \subdirName -> do
      it ("should produce the same snippets for all files in " <> subdirName <> "/*.miros") do
        let subdir = path <> "/" <> subdirName
        children <- readdir subdir

        -- Filter out directories
        files <- flip Array.filterA children \fileName -> do
          stats <- stat $ subdir <> "/" <> fileName
          pure $ isFile stats

        results <- for files \fileName -> do
          -- Compiler the contents
          result <- runExceptT do
            parsed <- resolveImport subdir fileName

            compiled <- liftEither $ sequence (compile parsed)
            pure compiled

          case result of
            Right snippets -> pure $ { fileName, snippets }
            Left err -> throwError $ error $ pretty err

        let paired = Array.zip results (Array.drop 1 results)
        for_ paired \(a /\ b) -> do
          let
            handleError err =
              fail $ fold
                [ "An error occurred while comparing "
                , a.fileName
                , " and "
                , b.fileName
                , " in directory "
                , subdir
                , "\n"
                , indentString 2 $ message err
                ]

          flip catchError handleError $ a.snippets `shouldEqual` b.snippets

main :: Effect Unit
main = launchAff_ do
  shouldParseExpressions <- shouldAllParse "./test/inputs/expr/good" $ parseMultilineExpression None
  shouldntParseExpressions <- noneShouldParse "./test/inputs/expr/bad" $ parseMultilineExpression None
  shouldParseToplevel <- shouldAllParse "./test/inputs/toplevel/good" parseToplevel
  shouldntParseToplevel <- noneShouldParse "./test/inputs/toplevel/bad" parseToplevel
  shouldMatch <- shouldAllMatch "./test/inputs/matches"

  runSpec [ consoleReporter ] do
    describe "parser" do
      describe "expressions" do
        shouldParseExpressions
        shouldntParseExpressions
      describe "toplevel" do
        shouldParseToplevel
        shouldntParseToplevel
    describe "interpreter" do
      shouldMatch

