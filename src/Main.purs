module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Either (Either(..))
import Emmet as Emmet
import Text.Parsing.Parser (ParseError, runParser)

p :: String -> Either ParseError String
p s = map Emmet.ppEmmet (runParser s Emmet.parseEmmet)

h :: String -> Either ParseError String
h s = map Emmet.emmetToHalogen (runParser s Emmet.parseEmmet)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  case p "div.lolClass#waow" of
    Left err -> logShow err
    Right emmet -> log emmet
