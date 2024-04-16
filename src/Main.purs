module Main where

import Miros.Prelude

import ArgParse.Basic (ArgParser, argument, choose, command, flagHelp, parseArgs, printArgError)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.String as String
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class.Console as Log
import Miros.Execution (compile)
import Miros.Generator.Luasnip (LuasnipGenConfig, generateLuasnipFile)
import Miros.Parser.Implementation (parseToplevel)
import Miros.Parser.Lib (runParser)
import Miros.Parser.Pieces (eof, optWs)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, writeTextFile)
import Node.Process (argv, setExitCode)

-- {{{ Basic CLI types
data Generator = Luasnip LuasnipGenConfig

data Command = Generate
  { generator :: Generator
  , input :: String
  , output :: String
  }

-- }}}
-- {{{ CLI arg parser
generator :: ArgParser Generator
generator = choose "generator"
  [ command [ "luasnip" ] "Generate lua snippet usable by luasnip" do
      flagHelp *> ado
        runtimeModule <- argument [ "--runtime", "-r" ] "The lua module additional functions should be imported from "
        in Luasnip { runtimeModule }
  ]

parser :: ArgParser Command
parser = choose "command"
  [ command [ "generate" ] "Compile a set of .miros files" do
      flagHelp *> ado
        input <- argument [ "--input", "-i" ] "Directory to read .miros files from"
        output <- argument [ "--output", "-o" ] "Directory to write generated files to"
        generator <- generator
        in Generate { generator: generator, input, output }
  ]

-- }}}
-- {{{ CLI runner
app :: Array String -> ExceptT String Aff Unit
app arguments = do
  command <- liftEither $ lmap printArgError $ parseArgs
    "miros"
    "Miros — a snippet generation language"
    parser
    arguments
  case command of
    Generate config -> do
      inputs <- liftAff $ readdir config.input
      for_ inputs \input -> do
        let
          handleError err =
            throwError $ fold
              [ "An error occurred while processing "
              , input
              , "\n"
              , indentString 2 err
              ]

        flip catchError handleError do
          contents <- liftAff $ readTextFile UTF8 $ config.input <> "/" <> input

          parsed <- liftEither
            $ lmap pretty
            $ runParser contents (parseToplevel <* optWs <* eof)
          compiled <- liftEither $ sequence (compile parsed)
          generated <- case config.generator of
            Luasnip luasnipConfig ->
              liftEither $ generateLuasnipFile luasnipConfig compiled

          void $ liftAff $ runCommand $ "mkdir -p " <> config.output
          log $ "🚀 Generated " <> show (Array.length compiled) <> " snippets for " <> input

          let
            outputExtension = case config.generator of
              Luasnip _ -> "lua"
            output = String.replace (String.Pattern ".miros")
              (Replacement $ "." <> outputExtension)
              input

          let outputPath = config.output <> "/" <> output
          liftAff $ writeTextFile UTF8 outputPath generated
          void $ liftAff $ runCommand $ "stylua " <> outputPath

main :: Effect Unit
main = do
  arguments <- argv <#> Array.drop 1
  launchAff_ do
    result <- runExceptT (app arguments)
    case result of
      Left err -> do
        Log.error err
        liftEffect $ setExitCode 1
      Right _ -> pure unit

-- }}}
-- {{{ Basic typeclass instances
derive instance Generic Generator _
derive instance Generic Command _

instance Debug Generator where
  debug = genericDebug

instance Debug Command where
  debug = genericDebug

-- }}}
-- {{{ Some FFI
foreign import runCommandImpl :: String -> EffectFnAff String

runCommand :: String -> Aff String
runCommand = fromEffectFnAff <<< runCommandImpl

-- }}}
