module Miros.Ast where

import Miros.Prelude

-- {{{ Basic types 
type Name = String
type Index = Int

type Modifiers =
  { positive :: Array String
  , negative :: Array String
  }

type Block =
  { elements :: Scope
  , modifiers :: Modifiers
  }

data TriggerKind = String | Pattern
type Trigger = TriggerKind /\ Expr

type Snippet =
  { triggerKind :: TriggerKind
  , trigger :: Expr
  , expansion :: Expr
  , name :: Maybe Expr
  , description :: Maybe Expr
  }

data SnippetCommand
  = SetExpansion Expr
  | SetName Expr
  | SetDescription Expr

data Expr
  = Literal String
  | Var Name
  | TabStop Index
  | CaptureGroupRef Index
  | Concat (Array Expr)
  | Array (Array Expr)
  | ArrayIndex Name (Array Expr)
  | Nonempty Index Expr
  | Choice Index (Array Expr)
  | Escape Expr
  | Empty
  | Newline

data Toplevel
  = Block Block
  | For Name Expr
  | Snippet Snippet

type Scope = Array Toplevel

-- }}}
-- {{{ Typeclass instance
derive instance Eq Expr
derive instance Generic Expr _
instance Debug Expr where
  debug e = genericDebug e

derive instance Eq TriggerKind
derive instance Generic TriggerKind _
instance Debug TriggerKind where
  debug e = genericDebug e

derive instance Eq Toplevel
derive instance Generic Toplevel _
instance Debug Toplevel where
  debug e = genericDebug e

derive instance Eq SnippetCommand
derive instance Generic SnippetCommand _
instance Debug SnippetCommand where
  debug e = genericDebug e

-- }}}
-- {{{ Helpers
type IncompleteSnippet =
  { expansion :: Maybe Expr
  , name :: Maybe Expr
  , description :: Maybe Expr
  }

makeSnippet :: Trigger -> List SnippetCommand -> Either String Snippet
makeSnippet (triggerKind /\ trigger) commands = do
  let initial = { expansion: Nothing, name: Nothing, description: Nothing }
  incomplete <- foldM go initial commands
  expansion <- note "Snippet expansion missing" incomplete.expansion
  pure $
    { triggerKind
    , trigger
    , expansion
    , name: incomplete.name
    , description: incomplete.description
    }
  where
  go :: IncompleteSnippet -> SnippetCommand -> Either String IncompleteSnippet
  go incomplete (SetExpansion expansion) = case incomplete.expansion of
    Just existing -> throwError $ fold
      [ "duplicate expansion: "
      , pretty existing
      , " and "
      , pretty expansion
      ]
    Nothing -> pure $ incomplete { expansion = Just expansion }
  go incomplete (SetName name) = case incomplete.name of
    Just existing -> throwError $ fold
      [ "duplicate name: "
      , pretty existing
      , " and "
      , pretty name
      ]
    Nothing -> pure $ incomplete { name = Just name }
  go incomplete (SetDescription description) = case incomplete.description of
    Just existing -> throwError $ fold
      [ "duplicate description: "
      , pretty existing
      , " and "
      , pretty description
      ]
    Nothing -> pure $ incomplete { description = Just description }
-- }}}
