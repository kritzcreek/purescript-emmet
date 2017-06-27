module Emmet.Parser.InputElement where

import Emmet.Parser.Data
import Prelude

import Control.Alt ((<|>))
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
import Emmet.Parser.Element (parseElementName, parseClass, parseId, parseGeneralStringAttribute)
import Emmet.Types (Attribute(..), Emmet, child, element, multiplication, sibling, InputType)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String (string, char, oneOf, satisfy)
import Text.Parsing.Parser.Token (alphaNum)

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

parseTypeInput :: String -> EmmetParser Attribute
parseTypeInput = case _ of
  "input" -> string "[type=" *> oneOf ['\'','"'] *> (TypeInputType <$> inputType) <* oneOf ['\'','"'] <* char ']'
  _ -> fail "Element is not <input>"

parseInputElement :: EmmetParser Emmet
parseInputElement = try $ do
  name <- parseElementName
  if name == "input"
    then do
      attributes <- many ((parseTypeInput name) <|> parseClass <|> parseId <|> parseGeneralStringAttribute)
      pure $ element name attributes
    else fail "Element is not <input>"
