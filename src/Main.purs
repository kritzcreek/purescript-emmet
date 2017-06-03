module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Emmet as Emmet
import Emmet.Halogen (renderHalogen)
import Text.Parsing.Parser (ParseError, runParser)

p :: String -> Either ParseError String
p s = map Emmet.ppEmmet (runParser s Emmet.parseEmmet)

h :: String -> String
h s =
  either show id
    (map (intercalate "\n" <<< map Emmet.renderHtmlBuilder)
     (map Emmet.evalEmmet (runParser s Emmet.parseEmmet)))

hal ∷ String → String
hal s =
  either show id
    (map (intercalate "\n" <<< map renderHalogen)
     (map Emmet.evalEmmet (runParser s Emmet.parseEmmet)))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  case p "div.lolClass#waow" of
    Left err -> logShow err
    Right emmet -> log emmet
