module Emmet.Parser where

import Emmet.Parser.Data (EmmetParser)
import Prelude (bind, pure, (*>), (<$>), (<<<))
import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.Char.Unicode (isDigit)
import Data.Foldable (class Foldable)
import Data.Int as Int
import Data.List (some)
import Data.Maybe (maybe)
import Data.String (fromCharArray)
import Emmet.Parser.Element (parseElement)
import Emmet.Parser.InputElement (parseInputElement)
import Emmet.Types (Emmet, child, multiplication, sibling, climbUp, text)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String (anyChar, char, satisfy)

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = fromCharArray <<< Array.fromFoldable

parseChild :: Emmet -> EmmetParser Emmet
parseChild e = child e <$> (char '>' *> parseEmmet)

parseClimbUp :: Emmet -> EmmetParser Emmet
parseClimbUp e = climbUp e <$> (char '^' *> parseEmmet)

parseSibling :: Emmet -> EmmetParser Emmet
parseSibling e = sibling e <$> (char '+' *> parseEmmet)

parseMultiplication :: Emmet -> EmmetParser Emmet
parseMultiplication e = do
  sInt <- fromCharList <$> (char '*' *> some (satisfy isDigit))
  repetitions <- maybe (fail "Failed to parse Multiplication number") pure (Int.fromString sInt)
  pure (multiplication e repetitions)

parseText :: EmmetParser Emmet
parseText = text <$> (char '{' *> (fromCharList <$> manyTill (anyChar) (char '}')))

parseLevel :: Emmet -> EmmetParser Emmet
parseLevel root = P.choice
   [ defer \_ -> parseChild root
   , defer \_ -> parseSibling root
   , defer \_ -> parseClimbUp root
   , defer \_ -> do
      e <- parseMultiplication root
      P.choice
        [ defer \_ -> parseChild e
        , defer \_ -> parseSibling e
        , defer \_ -> parseClimbUp e
        , pure e
        ]
   , pure root
   ]

parseEmmet :: EmmetParser Emmet
parseEmmet = do
  root <- (defer \_ -> P.between (char '(') (char ')') parseEmmet) <|>
    parseInputElement <|>
    parseElement <|>
    parseText

  parseLevel root
