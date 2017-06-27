module Emmet.Eval where

import Prelude

import Control.Biapply (biapply)
import Control.Bind ((>=>))
import Control.Monad.Free (liftF)
import DOM.HTML.Types (HTMLElement)
import Data.Array (foldr)
import Data.Array as Array
import Data.Foldable (fold, foldMap, intercalate)
import Data.Functor.Nu (Nu, observe)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.String (Pattern(Pattern), split)
import Data.Tuple (Tuple(..))
import Emmet.Types (Attribute, Emmet, EmmetF(..), InputType, climbUpTransform, getClass, getId, getInputType, getStringAttribute, textContentTransform)
import Matryoshka as M

evalEmmet :: Emmet -> NE.NonEmptyList HtmlBuilder
evalEmmet e = (textContentTransform (climbUpTransform e)) # M.cata case _ of
  Child os c -> setBuildChildren (NE.toList c) <$> os
  ClimbUp p c -> p <> c
  Sibling a b -> a <> b
  Multiplication a n ->
    foldr (<>) a (Array.replicate (n - 1) a)
  Element name attrs ->
    NE.singleton (htmlBuilder (List.singleton $
      HTMLElement { name, attributes: attributesToHtml attrs, children: List.Nil }))
  Text t ->
    NE.singleton (htmlBuilder (List.singleton $ HTMLText t))

-- climbUp :: List HtmlBuilder -> HtmlBuilder -> HtmlBuilder
-- climbUp l =

attributesToHtml :: List Attribute -> List HtmlAttribute
attributesToHtml attrs =
  let
    classes = List.mapMaybe getClass attrs
    ids = List.mapMaybe getId attrs
    inputTypes = map HtmlTypeInput (List.mapMaybe getInputType attrs)
    stringAttributes = map (\(Tuple a b) -> HtmlStringAttribute a b) (List.mapMaybe getStringAttribute attrs)
    htmlClasses =
      case List.uncons classes of
        Nothing -> List.Nil
        Just {head, tail: List.Nil} ->
          List.singleton (HtmlClass head)
        _ ->
          List.singleton (HtmlClasses classes)
    htmlId = maybe List.Nil (List.singleton <<< HtmlId) (List.head ids)
  in
    htmlId <> htmlClasses <> inputTypes <> stringAttributes

data HtmlAttribute
  = HtmlId String
  | HtmlClass String
  | HtmlClasses (List String)
  | HtmlTypeInput InputType
  | HtmlStringAttribute String String

renderHtmlAttribute :: HtmlAttribute -> String
renderHtmlAttribute = case _ of
  HtmlId i -> "id=" <> show i
  HtmlClass c -> "class=" <> show c
  HtmlClasses cs ->
    "class=\"" <> intercalate " " cs <> "\""
  HtmlTypeInput t ->
    "type=\"" <> (show t) <> "\""
  HtmlStringAttribute name val ->
    name <> "=\"" <> (show val) <> "\""

data HtmlBuilderF a = HtmlBuilderF (List (Node a))

data Node a
  = HTMLElement
    { name :: String
    , attributes :: List HtmlAttribute
    , children :: List a
    }
  | HTMLText String

derive instance functorNode :: Functor Node
derive instance functorHtmlBuilderF :: Functor HtmlBuilderF

type HtmlBuilder = Nu HtmlBuilderF

htmlBuilder :: List (Node HtmlBuilder) -> HtmlBuilder
htmlBuilder cs = M.embed (HtmlBuilderF cs)

mapNode :: forall a b. (List a -> List b) -> Node a -> Node b
mapNode f (HTMLElement node) = HTMLElement $ node { children = f node.children }
mapNode f (HTMLText s) = (HTMLText s)

setBuildChildren :: List HtmlBuilder -> HtmlBuilder -> HtmlBuilder
setBuildChildren cs builder =
  M.project builder # \(HtmlBuilderF nodes) ->
   map (mapNode (const cs)) nodes
   # htmlBuilder

renderHtmlBuilder :: HtmlBuilder -> String
renderHtmlBuilder = M.cata case _ of
  HtmlBuilderF nodes ->
    nodes
      <#> (case _ of
        HTMLElement {name, attributes, children} ->
          "<" <> name <> " "
          <> intercalate " " (map renderHtmlAttribute attributes)
          <> ">\n"
          <> intercalate "\n" (map (pad 2) children)
          <> "\n<" <> name <> "/>"
        HTMLText s -> s
      )
      # List.intercalate "\n"

pad :: Int -> String -> String
pad n s = s
  # split (Pattern "\n")
  <#> append (fold (Array.replicate n " "))
  # intercalate "\n"
