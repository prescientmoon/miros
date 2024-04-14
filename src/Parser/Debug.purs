  module Miros.Parser.Debug (bind, discard) where

import Miros.Parser.Lib as P
import Miros.Prelude as D

bind :: forall a b. D.Debug a => P.Parser a -> (a -> P.Parser b) -> P.Parser b
bind pa f = D.do
  a <- pa
  P.localLog (D.pretty a)
  f a

discard
  :: forall a b
   . D.Discard a
  => D.Debug a
  => P.Parser a
  -> (a -> P.Parser b)
  -> P.Parser b
discard pa f = D.do
  a <- pa
  P.localLog (D.pretty a)
  f a

