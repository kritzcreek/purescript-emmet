module Emmet.Parser where

import Prelude

import Control.Alt (($>), (<|>))
import Control.Alternative ((*>), (<*))
import Control.Lazy (defer)
import DOM.HTML.Indexed.InputType (InputType(..)) as IT
import Data.Array as Array
import Data.Char.Unicode (isAlpha, isAlphaNum, isDigit)
import Data.Foldable (class Foldable)
import Data.Int as Int
import Data.List (many, some)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.String (fromCharArray, trim)
import Emmet.Types (Attribute(..), Emmet, child, element, multiplication, sibling, InputType)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String (string, char, oneOf, satisfy)
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
classChar = satisfy (isAlpha || (==) '-' || (==) '_')

parseClass :: EmmetParser Attribute
parseClass = char '.' *> (Class <<< fromCharList <$> some classChar)

parseTypeInput :: String -> EmmetParser Attribute
parseTypeInput = case _ of
  "input" -> string "[type=" *> oneOf ['\'','"'] *> (TypeInputType <$> inputType) <* oneOf ['\'','"'] <* char ']'
  _ -> fail "Element was not an <input>"

parseId :: EmmetParser Attribute
parseId = char '#' *> (Id <<< fromCharList <$> some classChar)

inputType :: EmmetParser InputType
inputType = wrap <$> P.choice [
  (string "button" $> IT.InputButton),
  (string "checkbox" $> IT.InputCheckbox),
  (string "color" $> IT.InputColor),
  (string "date" $> IT.InputDate),
  (string "datetime" $> IT.InputDatetime),
  (string "datetime-local" $> IT.InputDatetimeLocal),
  (string "email" $> IT.InputEmail),
  (string "file" $> IT.InputFile),
  (string "hidden" $> IT.InputHidden),
  (string "image" $> IT.InputImage),
  (string "month" $> IT.InputMonth),
  (string "number" $> IT.InputNumber),
  (string "password" $> IT.InputPassword),
  (string "radio" $> IT.InputRadio),
  (string "range" $> IT.InputRange),
  (string "reset" $> IT.InputReset),
  (string "search" $> IT.InputSearch),
  (string "submit" $> IT.InputSubmit),
  (string "tel" $> IT.InputTel),
  (string "text" $> IT.InputText),
  (string "time" $> IT.InputTime),
  (string "url" $> IT.InputUrl),
  (string "week" $> IT.InputWeek)]

parseElement :: EmmetParser Emmet
parseElement = do
  name <- parseElementName
  attributes <- many (parseClass <|> parseId <|> (parseTypeInput name))
  pure $ element name attributes

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
