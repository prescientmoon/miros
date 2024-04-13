module Miros.Helpers.Array (dropEndWhile) where

import Miros.Prelude
import Data.Array as Array

dropEndWhile :: forall a. (a -> Boolean) -> Array a -> Array a
dropEndWhile p = Array.reverse >>> Array.dropWhile p >>> Array.reverse
