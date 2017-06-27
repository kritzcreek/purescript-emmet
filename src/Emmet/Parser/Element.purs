module Emmet.Parser.Element where

import Prelude
import Emmet.Parser.Data (EmmetParser)
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Char.Unicode (isAlphaNum)
import Data.Foldable (class Foldable)
import Data.List (many, some)
import Data.String (fromCharArray)
import Emmet.Types (Emmet, element)
import Emmet.Attribute (Attribute(StringAttribute, TextContent, Id, Class))
import Text.Parsing.Parser.Combinators (manyTill)
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

parseTextContent :: EmmetParser Attribute
parseTextContent = char '{' *> (TextContent <<< fromCharList <$> manyTill (anyChar) (char '}'))

parseGeneralStringAttribute :: EmmetParser Attribute
parseGeneralStringAttribute = do
  attr <- char '[' *> (P.choice $ map string [
      "alt", "charset", "for", "href", "name", "rel", "src",
      "target", "title", "action", "value", "placeholder", "pattern",
      "poster"
    ])
  wrapper <- char '=' *> oneOf ['\'','"']
  val <- manyTill (anyChar) (char wrapper)
  void $ char ']'
  pure $ StringAttribute attr (fromCharList val)

parseElement :: EmmetParser Emmet
parseElement = element <$> parseElementName <*> many (
    parseTextContent <|>
    parseClass <|>
    parseId <|>
    parseGeneralStringAttribute
  )
