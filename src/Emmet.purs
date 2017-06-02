module Emmet where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.Char.Unicode (isDigit)
import Data.Foldable (class Foldable)
import Data.Functor.Nu (Nu)
import Data.Int as Int
import Data.List (List, many, some)
import Data.List as List
import Data.Maybe (maybe)
import Data.String (fromCharArray)
import Matryoshka as M
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String (char, satisfy)
import Text.Parsing.Parser.Token (alphaNum)

data Attribute
  = Class String
  | Id String

instance showAttribute :: Show Attribute where
  show = case _ of
    Class s -> "(Class " <> s <> ")"
    Id s -> "(Id " <> s <> ")"

data EmmetF a
  = Child a a
  | Parent a a
  | Sibling a a
  | Multiplication a Int
  | Element String (List Attribute)

element :: String -> List Attribute -> Emmet
element s as = M.embed (Element s as)

child :: Emmet -> Emmet -> Emmet
child a b = M.embed (Child a b)

parent :: Emmet -> Emmet -> Emmet
parent a b = M.embed (Parent a b)

sibling :: Emmet -> Emmet -> Emmet
sibling a b = M.embed (Sibling a b)

multiplication :: Emmet -> Int -> Emmet
multiplication a b = M.embed (Multiplication a b)

ppEmmet :: Emmet -> String
ppEmmet = M.cata case _ of
  Child a b -> "(Child " <> a <> " " <> b <> ")"
  Parent a b -> "(Parent " <> a <> " " <> b <> ")"
  Sibling a b -> "(Sibling " <> a <> " " <> b <> ")"
  Multiplication a n -> "(Multiplication " <> a <> " " <> show n <> ")"
  Element name attrs -> "(Element " <> name <> " " <> show attrs <> ")"

derive instance functorEmmetF :: Functor EmmetF

type Emmet = Nu EmmetF

type EmmetParser a = Parser String a

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = fromCharArray <<< Array.fromFoldable

parseElementName :: EmmetParser String
parseElementName = fromCharList <$> some alphaNum

parseChild :: Emmet -> EmmetParser Emmet
parseChild e = child e <$> (char '>' *> parseEmmet)

parseParent :: Emmet -> EmmetParser Emmet
parseParent e = parent e <$> (char '^' *> parseEmmet)

parseSibling :: Emmet -> EmmetParser Emmet
parseSibling e = sibling e <$> (char '+' *> parseEmmet)

parseMultiplication :: Emmet -> EmmetParser Emmet
parseMultiplication e = do
  sInt <- fromCharList <$> (char '*' *> some (satisfy isDigit))
  repetitions <- maybe (fail "Failed to parse Multiplication number") pure (Int.fromString sInt)
  pure (multiplication e repetitions)

parseClass :: EmmetParser Attribute
parseClass = char '.' *> (Class <<< fromCharList <$> some alphaNum)

parseId :: EmmetParser Attribute
parseId = char '#' *> (Id <<< fromCharList <$> some alphaNum)

parseElement :: EmmetParser Emmet
parseElement = element <$> parseElementName <*> many (parseClass <|> parseId)

parseEmmet :: EmmetParser Emmet
parseEmmet = do
  root <- parseElement
  P.choice
     [ defer \_ -> parseChild root
     , defer \_ -> parseSibling root
     , defer \_ -> parseParent root
     , defer \_ -> parseMultiplication root
     , pure root
     ]

-- to halogen

emmetToHalogen :: Emmet -> String
emmetToHalogen = M.cata case _ of
  Child _ _ -> ""
  Parent _ _ -> ""
  Sibling _ _ -> ""
  Multiplication _ _ -> ""
  Element name attrs
    | List.null attrs -> "HH." <> name <> "_ [ ]"
    | otherwise -> "HH." <> name <> "[ ] [ ]"
