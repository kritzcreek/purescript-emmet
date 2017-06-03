module Emmet
  ( module Emmet
  , module ETypes
  , module EParser
  ) where

import Prelude

import Data.Array (foldr)
import Data.Array as Array
import Data.Foldable (class Foldable, fold, intercalate)
import Data.Functor.Nu (Nu)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (Pattern(Pattern), split)
import Emmet.Parser as EParser
import Emmet.Types (Attribute, Emmet, EmmetF(..), getClass, getId)
import Emmet.Types as ETypes
import Matryoshka as M

evalEmmet :: Emmet -> NE.NonEmptyList HtmlBuilder
evalEmmet = M.cata case _ of
  Child os c -> setBuildChildren (NE.toList c) <$> os
  Sibling a b -> a <> b
  Multiplication a n ->
    foldr (<>) a (Array.replicate (n - 1) a)
  Element name attrs ->
    NE.singleton (htmlBuilder (List.singleton
      { name, attributes: attributesToHtml attrs, children: List.Nil }))

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
