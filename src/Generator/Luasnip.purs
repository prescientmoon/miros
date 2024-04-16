module Miros.Generator.Luasnip (LuasnipGenConfig, generateLuasnipFile) where

import Miros.Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.HashMap as HM
import Data.HashSet as HS
import Data.String as String
import Miros.Ast as Ast
import Miros.Evaluation.EvaluationContext (NormalizedSnippet)
import Miros.Evaluation.Expansion as EX

type LuasnipGenConfig =
  { runtimeModule :: String
  }

luaTable :: Array (String /\ String) -> String
luaTable props = "{\n"
  <> inner
  <> "}"
  where
  inner = props
    <#> uncurry (\key value -> key <> "= " <> value)
    # String.joinWith ","

luaArray :: Array String -> String
luaArray elements = "{"
  <> String.joinWith "," elements
  <> "}"

luaString :: String -> String
luaString = show -- This is not a perfect solution, but oh well

luaCall :: String -> Array String -> String
luaCall fn args = fn <> "(" <> String.joinWith "," args <> ")"

luaNil :: String
luaNil = "nil"

generateLuasnipFile
  :: LuasnipGenConfig
  -> Array NormalizedSnippet
  -> Either String String
generateLuasnipFile config snippets = do
  let
    header =
      """
      local ls = require("luasnip")
      local extras = require("luasnip.extras")
      local conditions = require("luasnip.extras.expand_conditions")
      local s = ls.snippet
      local t = ls.text_node
      local i = ls.insert_node
      local c = ls.choice_node
      local l = extras.lambda
      local f = ls.function_node
      local sn = ls.snippet_node
      local n = extras.nonempty
      local extra_conditions = 
      """ <> luaCall "require"
        [ luaString config.runtimeModule ]
  snippets <- traverse generateLuasnipSnippet snippets
  pure $ header <> "\nreturn " <> luaArray snippets

generateLuasnipSnippet :: NormalizedSnippet -> Either String String
generateLuasnipSnippet snip = do
  let
    handleError err = fold
      [ "While generating luasnip code for snippet "
      , snip.name
      , ":\n"
      , indentString 2 err
      ]

  snippetTable <- lmap handleError $ expand snip.expansion

  pure $ luaCall "s"
    [ propTable
    , snippetTable
    ]

  where
  propTable = luaTable $ fold
    [ [ "name" /\ luaString snip.name
      , "trig" /\ luaString snip.trigger
      ]
    , descriptionProp
    , trigEngineProp
    , snippetProp
    , conditionProp
    ]

  descriptionProp = case snip.description of
    Just desc -> [ "desc" /\ luaString desc ]
    Nothing -> []
  trigEngineProp = case snip.triggerKind of
    Ast.Pattern -> [ "trigEngine" /\ luaString "pattern" ]
    Ast.String -> []
  snippetProp = case HM.lookup "auto" snip.modifiers of
    Just true -> [ "snippetType" /\ luaString "autosnippet" ]
    _ -> []
  conditionProp =
    if Array.null conditionModifiers then
      []
    else
      [ "condition" /\ String.joinWith " + " conditions ]
    where
    conditionModifiers = HM.toArrayBy (/\) $ HM.delete "auto" snip.modifiers
    { yes, no } = Array.partition snd conditionModifiers
    mkCondition = fst >>> case _ of
      "start" -> "conditions.line_begin"
      "end" -> "conditions.line_end"
      "select" -> "conditions.has_selected_text"
      other -> "extra_conditions." <> other
    conditions = (yes <#> mkCondition) <> (no <#> mkCondition <#> (<>) "-")

type ExpansionState =
  -- | It turns out luasnip doesn't like it when we
  -- | reuse the same jump-index multiple time, so we do it once
  -- | and use lambda nodes to mirror the text for the rest of the appearences.
  { createdTabstops :: HashSet Int
  }

type ExpandM = StateT ExpansionState (Either String)

initialState :: ExpansionState
initialState = { createdTabstops: HS.empty }

expand :: EX.Expansion -> Either String String
expand expansion = expansionToLua 0 expansion
  # flip evalStateT initialState

expansionToLua :: Int -> EX.Expansion -> ExpandM String
expansionToLua offset expansion = expansion
  # EX.chunks
  # traverse (chunkToLua offset)
  <#> luaArray

-- | Mirros the previous text if the jump-index has already been used
mirrorOnReuse :: Int -> String -> ExpandM String
mirrorOnReuse i default = do
  created <- gets _.createdTabstops

  if HS.member i created then do
    when (i == 0) do
      throwError "Cannot use more than one final tabstop"

    pure $ luaCall "l" [ "l._" <> show i, show i ]
  else do
    modify_ _ { createdTabstops = HS.insert i created }
    pure default

-- | Errors out if the given jump-index has already been used
errorOnReuse :: Int -> String -> ExpandM String
errorOnReuse i default = do
  created <- gets _.createdTabstops

  if HS.member i created then
    throwError $ "Cannot use index " <> show i <> " more than once"
  else do
    modify_ _ { createdTabstops = HS.insert i created }
    pure default

chunkToLua :: Int -> EX.ExpansionChunk -> ExpandM String
chunkToLua offset = case _ of
  EX.Literal text -> pure $ luaCall "t"
    $ pure
    $ luaArray
    $ luaString
    <$> String.split (String.Pattern "\n") text
  EX.TabStop i -> mirrorOnReuse (i - offset) $
    luaCall "i" [ show $ i - offset ]
  EX.CaptureGroupRef i -> errorOnReuse (i - offset) $
    luaCall "f"
      [ "function(_, snip) return snip.captures[" <> show (i + 1) <> "] end"
      ]
  EX.Choice i options -> do
    luaOptions <- for options \option -> do
      expansion <- throwawayState do
        put initialState
        expansionToLua (i - 1 - offset) option
      pure $ luaCall "sn" [ luaNil, expansion ]
    errorOnReuse (i - offset) $
      luaCall "c" [ show $ i - offset, luaArray luaOptions ]
  EX.Nonempty i inner
    | [ EX.Literal lit ] <- EX.chunks inner ->
        pure $ luaCall "n"
          [ show $ i - offset
          , luaString lit
          , luaString ""
          ]
  whole -> throwError $ "Cannot generate luasnip snippet out of construct " <> pretty whole
