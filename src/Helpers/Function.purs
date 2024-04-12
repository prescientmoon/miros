module Miros.Helpers.Function (bind) where

import Miros.Prelude

bind :: forall a b. (a -> b) -> a -> b
bind = ($)
