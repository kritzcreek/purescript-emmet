module Emmet.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.Char.Unicode (isAlphaNum, isDigit)
import Data.Foldable (class Foldable)
import Data.Int as Int
import Data.List (many, some)
import Data.Maybe (maybe)
import Data.String.CodeUnits (fromCharArray)
import Emmet.Types (Attribute(..), Emmet, child, element, multiplication, sibling)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String (char, satisfy)
import Text.Parsing.Parser.Token (alphaNum)

type EmmetParser a = Parser String a

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = fromCharArray <<< Array.fromFoldable

parseElementName :: EmmetParser String
parseElementName = fromCharList <$> some alphaNum

parseChild :: Emmet -> EmmetParser Emmet
parseChild e = child e <$> (char '>' *> parseEmmet)

parseSibling :: Emmet -> EmmetParser Emmet
parseSibling e = sibling e <$> (char '+' *> parseEmmet)

parseMultiplication :: Emmet -> EmmetParser Emmet
parseMultiplication e = do
  sInt <- fromCharList <$> (char '*' *> some (satisfy isDigit))
  repetitions <- maybe (fail "Failed to parse Multiplication number") pure (Int.fromString sInt)
  pure (multiplication e repetitions)

classChar :: EmmetParser Char
classChar = satisfy (isAlphaNum || (==) '-' || (==) '_')

parseClass :: EmmetParser Attribute
parseClass = char '.' *> (Class <<< fromCharList <$> some classChar)

parseId :: EmmetParser Attribute
parseId = char '#' *> (Id <<< fromCharList <$> some classChar)

parseElement :: EmmetParser Emmet
parseElement = explicitTag <|> implicitTag
  where
    explicitTag = element <$> parseElementName <*> many (parseClass <|> parseId)

    -- #page.full-width>header.bigger  --> div#page.full-width>header.bigger
    implicitTag = element <$> pure "div" <*> some (parseClass <|> parseId)

parseEmmet :: EmmetParser Emmet
parseEmmet = do
  root <- (defer \_ -> P.between (char '(') (char ')') parseEmmet) <|> parseElement
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
