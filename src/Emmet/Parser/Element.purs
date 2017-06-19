module Emmet.Parser.Element where

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
import Emmet.Types (Attribute(..), Emmet, child, element, multiplication, sibling, InputType)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, endBy, endBy1, manyTill, notFollowedBy, try)
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String (string, char, oneOf, satisfy, anyChar)
import Text.Parsing.Parser.Token (alphaNum)

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = fromCharArray <<< Array.fromFoldable

parseElementName :: EmmetParser String
parseElementName = fromCharList <$> some alphaNum

classChar :: EmmetParser Char
classChar = satisfy (isAlphaNum || (==) '-' || (==) '_')

parseClass :: EmmetParser Attribute
parseClass = char '.' *> (Class <<< fromCharList <$> some classChar)

parseId :: EmmetParser Attribute
parseId = char '#' *> (Id <<< fromCharList <$> some classChar)

parseGeneralStringAttribute :: EmmetParser Attribute
parseGeneralStringAttribute = do
  attr <- char '[' *> (P.choice $ map string [
      "alt", "charset", "for", "href", "name", "rel", "src",
      "target", "title", "action", "value", "placeholder", "pattern",
      "poster"
    ])
  wrapper <- char '=' *> oneOf ['\'','"']
  val <- manyTill (anyChar) (char wrapper)
  pure $ StringAttribute attr (fromCharList val)

parseElement :: EmmetParser Emmet
parseElement = element <$> parseElementName <*> many (parseClass <|> parseId <|> parseGeneralStringAttribute)
