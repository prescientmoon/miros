module Miros.Parser.Implementation (parser) where

import Miros.Prelude

import Miros.Parser.Lib as P
import Miros.Parser.Pieces as PP
import StringParser as SP

keyword :: String -> P.Parser Unit
keyword string = void $ P.indented $ SP.string string

intList :: P.Parser (List Int)
intList = cons loop
  where
  cons loop_ = Cons <$> p <*> P.withRelation P.IGt loop_
  loop = SP.fix \self -> cons self <|> nil
  nil = pure Nil
  p = PP.try $ PP.optWs *> PP.nat

block :: P.Parser (List (List Int))
block = do
  keyword "block"
  P.withRelation P.IGt
    $ PP.many1
    $ P.absolute
    $ P.withRelation P.IGt
    $ PP.try intList

parser :: P.Parser _
parser = block <* PP.optWs <* PP.eof
