module Emmet.Halogen where

import Emmet

import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.List (List(..), null, singleton, uncons, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains)
import Matryoshka as M
import Prelude (map, otherwise, show, (#), (<#>), (<>), (<@>))
import Text.Parsing.Parser (ParseError, runParser)
import DOM.HTML.Indexed.InputType (InputType(..)) as IT

indent ∷ Int
indent = 4

emmetHalogen ∷ String → Either ParseError String
emmetHalogen s = s
  # runParser <@> parseEmmet
  <#> evalEmmet
  <#> map renderHalogen
  <#> intercalate "\n"

renderHalogen ∷ HtmlBuilder → String
renderHalogen =
  M.cata case _ of
    HtmlBuilderF nodes →
      nodes <#> renderNode # intercalate ("\n")

renderNode ∷ Node String → String
renderNode {name, attributes, children} =
  case uncons attributes of
    Nothing → renderNoAttributes name children
    Just {head, tail} → renderWithAttributes name (head : tail) children

renderWithAttributes ∷ String → List HtmlAttribute → List String → String
renderWithAttributes name attributes children =
  "HH." <> name <> "\n"
    <> intercalate "\n" (map (pad indent) (renderList (map renderAttribute attributes)))
    <> "\n"
    <> intercalate "\n" (map (pad indent) (renderList children))
  where
    renderAttribute = case _ of
      HtmlId id → "HP.id " <> show id
      HtmlClass c → "HP.class_ (HH.ClassName " <> show c <> ")"
      HtmlClasses cs → "HP.classes (map HH.ClassName [ " <> intercalate ", " (map show cs) <> " ])"
      HtmlTypeInput t → "HP.type HP." <> (renderInputType t)
      HtmlStringAttribute name val → "HP." <> name <> " \"" <> val <> "\""

renderNoAttributes ∷ String → List String → String
renderNoAttributes name children
  | null children = "HH." <> name <> "_ []"
  | otherwise = "HH." <> name <> "_\n" <>
      intercalate "\n" (map (pad indent) (renderList children))

renderList ∷ List String → List String
renderList = case _ of
  Nil → singleton ("[ ]")
  head : Nil →
    if contains (Pattern "\n") head then
      ("[ " <> head) : singleton "]"
    else
      singleton ("[ " <> head <> " ]")
  head : tail →
    ("[ " <> head) : map (", " <> _) tail <> singleton ("]")

renderInputType ∷ InputType → String
renderInputType a = case (unwrap a) of
  IT.InputButton -> "InputButton"
  IT.InputCheckbox -> "InputCheckbox"
  IT.InputColor -> "InputColor"
  IT.InputDate -> "InputDate"
  IT.InputDatetime -> "InputDatetime"
  IT.InputDatetimeLocal -> "InputDatetimeLocal"
  IT.InputEmail -> "InputEmail"
  IT.InputFile -> "InputFile"
  IT.InputHidden -> "InputHidden"
  IT.InputImage -> "InputImage"
  IT.InputMonth -> "InputMonth"
  IT.InputNumber -> "InputNumber"
  IT.InputPassword -> "InputPassword"
  IT.InputRadio -> "InputRadio"
  IT.InputRange -> "InputRange"
  IT.InputReset -> "InputReset"
  IT.InputSearch -> "InputSearch"
  IT.InputSubmit -> "InputSubmit"
  IT.InputTel -> "InputTel"
  IT.InputText -> "InputText"
  IT.InputTime -> "InputTime"
  IT.InputUrl -> "InputUrl"
  IT.InputWeek -> "InputWeek"
