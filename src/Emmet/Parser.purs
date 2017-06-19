module Emmet.Parser where

import Emmet.Parser.Data
import Prelude

import Control.Alt (($>), (<|>))
import Control.Alternative (empty, when, (*>), (<*))
import Control.Lazy (defer)
import Control.MonadPlus (guard)
import DOM.HTML.Indexed.InputType (InputType(..)) as IT
import Data.Array as Array
import Data.Char.Unicode (isAlphaNum, isDigit)
import Data.Foldable (class Foldable)
import Data.Functor (voidLeft)
import Data.Int as Int
import Data.List (many, some)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.String (fromCharArray, trim)
import Emmet.Parser.Element (parseElement)
import Emmet.Parser.InputElement (parseInputElement)
import Emmet.Types (Attribute(..), Emmet, child, element, multiplication, sibling, InputType)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String (string, char, oneOf, satisfy)
import Text.Parsing.Parser.Token (alphaNum)

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = fromCharArray <<< Array.fromFoldable

parseChild :: Emmet -> EmmetParser Emmet
parseChild e = child e <$> (char '>' *> parseEmmet)

parseSibling :: Emmet -> EmmetParser Emmet
parseSibling e = sibling e <$> (char '+' *> parseEmmet)

parseMultiplication :: Emmet -> EmmetParser Emmet
parseMultiplication e = do
  sInt <- fromCharList <$> (char '*' *> some (satisfy isDigit))
  repetitions <- maybe (fail "Failed to parse Multiplication number") pure (Int.fromString sInt)
  pure (multiplication e repetitions)

parseEmmet :: EmmetParser Emmet
parseEmmet = do
  root <- (defer \_ -> P.between (char '(') (char ')') parseEmmet) <|>
    parseInputElement <|>
    parseElement

  P.choice
     [ defer \_ -> parseChild root
     , defer \_ -> parseSibling root
     , defer \_ -> do
          e <- parseMultiplication root
          P.choice
            [ defer \_ -> parseChild e
            , defer \_ -> parseSibling e
            , pure e
            ]
     , pure root
     ]
