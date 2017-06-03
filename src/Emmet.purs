module Emmet where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.Char.Unicode (isDigit)
import Data.Foldable (class Foldable, fold, intercalate)
import Data.Functor.Nu (Nu)
import Data.Int as Int
import Data.List (List, many, some)
import Data.List as List
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), fromCharArray, split)
import Data.Unfoldable (replicate)
import Matryoshka as M
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String (char, satisfy)
import Text.Parsing.Parser.Token (alphaNum)

data Attribute
  = Class String
  | Id String

getClass :: Attribute -> Maybe String
getClass = case _ of
  Class x -> Just x
  _ -> Nothing

getId :: Attribute -> Maybe String
getId = case _ of
  Id x -> Just x
  _ -> Nothing

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

derive instance functorEmmetF :: Functor EmmetF

type Emmet = Nu EmmetF

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
  root <- (defer \_ -> P.between (char '(') (char ')') parseEmmet) <|> parseElement
  P.choice
     [ defer \_ -> parseChild root
     , defer \_ -> parseSibling root
     , defer \_ -> parseParent root
     , defer \_ -> do
          e <- parseMultiplication root
          P.choice
            [ defer \_ -> parseChild e
            , defer \_ -> parseSibling e
            , defer \_ -> parseParent e
            , pure e
            ]
     , pure root
     ]

evalEmmet :: Emmet -> NE.NonEmptyList HtmlBuilder
evalEmmet = M.cata case _ of
  Child os c -> setBuildChildren (NE.toList c) <$> os
  -- Parent _ _ -> ?x
  Sibling a b -> a <> b
  Multiplication a n ->
    let next = NE.head a
    in fromMaybe (NE.singleton next) (NE.fromList (replicate n next))
  Element name attrs ->
    NE.singleton (htmlBuilder (List.singleton
      { name, attributes: attributesToHtml attrs, children: List.Nil }))
  _ -> NE.singleton testDiv

testDiv :: HtmlBuilder
testDiv =
  htmlBuilder
    (List.singleton
      {name: "test", attributes: List.Nil, children: List.Nil })

attributesToHtml :: List Attribute -> List HtmlAttribute
attributesToHtml attrs =
  let
    classes = List.mapMaybe getClass attrs
    ids = List.mapMaybe getId attrs
    htmlClasses =
      case List.uncons classes of
        Nothing -> List.Nil
        Just {head, tail: List.Nil} ->
          List.singleton (HtmlClass head)
        _ ->
          List.singleton (HtmlClasses classes)
    htmlId = maybe List.Nil (List.singleton <<< HtmlId) (List.head ids)
  in
    htmlId <> htmlClasses


data HtmlAttribute
  = HtmlId String
  | HtmlClass String
  | HtmlClasses (List String)

renderHtmlAttribute :: HtmlAttribute -> String
renderHtmlAttribute = case _ of
  HtmlId i -> "id=" <> show i
  HtmlClass c -> "class=" <> show c
  HtmlClasses cs ->
    "class=\"" <> List.intercalate " " cs <> "\""

renderHtmlAttributeHalogen :: HtmlAttribute -> String
renderHtmlAttributeHalogen = case _ of
  HtmlId i -> "HP.id_ " <> show i
  HtmlClass c -> "HP.class_ (HH.ClassName " <> show c <> ")"
  HtmlClasses cs ->
    "HP.classes [ "
      <> commaSep (map (\c -> "HH.ClassName " <> show c) cs)
      <> " ]"

data HtmlBuilderF a = HtmlBuilderF (List (Node a))

derive instance functorHtmlBuilderF :: Functor HtmlBuilderF

type HtmlBuilder = Nu HtmlBuilderF

htmlBuilder :: List (Node HtmlBuilder) -> HtmlBuilder
htmlBuilder cs = M.embed (HtmlBuilderF cs)

type Node a =
  { name :: String
  , attributes :: List HtmlAttribute
  , children :: List a
  }

mapNode :: forall a b. (List a -> List b) -> Node a -> Node b
mapNode f node = node { children = f node.children }

-- node :: String -> List HtmlAttribute -> 

setBuildChildren :: List HtmlBuilder -> HtmlBuilder -> HtmlBuilder
setBuildChildren cs builder =
  M.project builder # \(HtmlBuilderF nodes) ->
   map (mapNode (const cs)) nodes
   # htmlBuilder

commaSep :: forall f. Foldable f => f String -> String
commaSep = List.intercalate ", "

renderHtmlBuilder :: HtmlBuilder -> String
renderHtmlBuilder = M.cata case _ of
  HtmlBuilderF nodes ->
    nodes
      <#> (\ {name, attributes, children} ->
        "<" <> name <> " "
        <> List.intercalate " " (map renderHtmlAttribute attributes)
        <> ">\n"
        <> List.intercalate "\n" (map (pad 2) children)
        <> "\n<" <> name <> "/>")
      # List.intercalate "\n"

pad :: Int -> String -> String
pad n s = s
  # split (Pattern "\n")
  <#> append (fold (Array.replicate n " "))
  # intercalate "\n"

-- renderHalogen :: Html -> String
-- renderHalogen = M.cata case _ of
--   Text t ->
--     "HH.text " <> show t <> ""
--   SelfClosing s attrs ->
--     "HH." <> s <> " [ " <> commaSep (map renderHtmlAttribute attrs) <> " ]"
--   Node s attrs children
--     | List.null attrs ->
--       "HH." <> s
--       <> "_ [ " <> commaSep children <> " ]"
--     | otherwise ->
--       "HH." <> s
--       <> " [ " <> commaSep (map renderHtmlAttribute attrs)
--       <> " ] [ " <> commaSep children
--       <> " ]"
