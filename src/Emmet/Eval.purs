module Emmet.Eval where

import Prelude

import Data.Array (foldr)
import Data.Array as Array
import Data.Foldable (fold, intercalate)
import Data.Functor.Nu (Nu)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (Pattern(Pattern), split)
import Emmet.Types (Attribute, Emmet, EmmetF(..), getClass, getId)
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
    "class=\"" <> intercalate " " cs <> "\""

data HtmlBuilderF a = HtmlBuilderF (List (Node a))

type Node a =
  { name :: String
  , attributes :: List HtmlAttribute
  , children :: List a
  }

derive instance functorHtmlBuilderF :: Functor HtmlBuilderF

type HtmlBuilder = Nu HtmlBuilderF

htmlBuilder :: List (Node HtmlBuilder) -> HtmlBuilder
htmlBuilder cs = M.embed (HtmlBuilderF cs)

mapNode :: forall a b. (List a -> List b) -> Node a -> Node b
mapNode f node = node { children = f node.children }

setBuildChildren :: List HtmlBuilder -> HtmlBuilder -> HtmlBuilder
setBuildChildren cs builder =
  M.project builder # \(HtmlBuilderF nodes) ->
   map (mapNode (const cs)) nodes
   # htmlBuilder

renderHtmlBuilder :: HtmlBuilder -> String
renderHtmlBuilder = M.cata case _ of
  HtmlBuilderF nodes ->
    nodes
      <#> (\ {name, attributes, children} ->
        "<" <> name <> " "
        <> intercalate " " (map renderHtmlAttribute attributes)
        <> ">\n"
        <> intercalate "\n" (map (pad 2) children)
        <> "\n<" <> name <> "/>")
      # List.intercalate "\n"

pad :: Int -> String -> String
pad n s = s
  # split (Pattern "\n")
  <#> append (fold (Array.replicate n " "))
  # intercalate "\n"
