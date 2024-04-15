module Miros.Evaluation.EvaluationContext where

import Miros.Prelude

import Miros.Evaluation.Expansion (Expansion)
import Miros.Ast as Ast
import Data.HashSet as HS
import Data.HashMap as HM
import Data.Array as Array

type NormalizedSnippet =
  { name :: String
  , description :: Maybe String
  , trigger :: String
  , triggerKind :: Ast.TriggerKind
  , expansion :: Expansion
  , modifiers :: HashMap String Boolean
  }

-- De-Brujin variable index
type DBIndex = Int

-- De-Brujin variable lavel
type DBLevel = Int

type BoundVariable =
  { references :: HashSet DBLevel
  , boundBy :: Ast.Expr
  }

type Context =
  { scope :: Array BoundVariable
  , nameMap :: HashMap Ast.Name DBLevel
  , modifiers :: HashMap String Boolean
  }

type WorldlineChoice =
  { index :: Int
  , source :: Ast.Expr
  , value :: Expansion
  }

type Worldline = Array (Maybe WorldlineChoice)

references :: Ast.Expr -> HashSet Ast.Name
references (Ast.Var name) = HS.singleton name
references (Ast.Nonempty _ inner) = references inner
references (Ast.Escape inner) = references inner
references (Ast.Concat many) = HS.unions $ references <$> many
references (Ast.Choice _ inner) = HS.unions $ references <$> inner
references (Ast.Array inner) = HS.unions $ references <$> inner
references (Ast.ArrayIndex var inner) = HS.insert var $ HS.unions $ references <$> inner
references _ = HS.empty

-- | Given a set of names (the "direct dependencies"), this will transform those
-- | names to de-brujin levels, and then return them together with all their
-- | (possibly indirect) dependencies.
expandReferences :: Context -> HashSet Ast.Name -> HashSet DBLevel
expandReferences context names =
  let
    direct = names
      # HS.mapMaybe (flip HM.lookup context.nameMap)
    indirect = direct # HS.mapMaybe \level -> do
      entry <- Array.index context.scope level
      pure $ entry.references
  in
    HS.union direct $ HS.unions indirect

