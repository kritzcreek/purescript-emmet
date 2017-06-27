module Emmet.Attribute where

import Prelude
import DOM.HTML.Indexed.InputType (InputType, InputType(..), renderInputType) as IT
import Data.Foldable (foldl)
import Data.List (List, snoc)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))

data Attribute
  = Class String
  | Id String
  | TypeInputType InputType
  | StringAttribute String String
  | TextContent String

instance attributeEq :: Eq Attribute where
  eq (TypeInputType a) (TypeInputType b) = eq a b
  eq (Class a) (Class b) = eq a b
  eq (Id a) (Id b) = eq a b
  eq (StringAttribute a b) (StringAttribute c d) = (eq a c) && (eq b d)
  eq _ _ = false

newtype InputType = InputType IT.InputType
derive instance newtypeInputType :: Newtype InputType _

instance showInputType :: Show InputType where
  show (InputType a) = IT.renderInputType a

instance eqInputType :: Eq InputType where
  eq (InputType IT.InputButton) (InputType IT.InputButton) = true
  eq (InputType IT.InputCheckbox) (InputType IT.InputCheckbox) = true
  eq (InputType IT.InputColor) (InputType IT.InputColor) = true
  eq (InputType IT.InputDate) (InputType IT.InputDate)= true
  eq (InputType IT.InputDatetime) (InputType IT.InputDatetime) = true
  eq (InputType IT.InputDatetimeLocal) (InputType IT.InputDatetimeLocal) = true
  eq (InputType IT.InputEmail) (InputType IT.InputEmail) = true
  eq (InputType IT.InputFile) (InputType IT.InputFile) = true
  eq (InputType IT.InputHidden) (InputType IT.InputHidden) = true
  eq (InputType IT.InputImage) (InputType IT.InputImage) = true
  eq (InputType IT.InputMonth) (InputType IT.InputMonth) = true
  eq (InputType IT.InputNumber) (InputType IT.InputNumber) = true
  eq (InputType IT.InputPassword) (InputType IT.InputPassword) = true
  eq (InputType IT.InputRadio) (InputType IT.InputRadio) = true
  eq (InputType IT.InputRange) (InputType IT.InputRange) = true
  eq (InputType IT.InputReset) (InputType IT.InputReset) = true
  eq (InputType IT.InputSearch) (InputType IT.InputSearch) = true
  eq (InputType IT.InputSubmit) (InputType IT.InputSubmit) = true
  eq (InputType IT.InputTel) (InputType IT.InputTel) = true
  eq (InputType IT.InputText) (InputType IT.InputText) = true
  eq (InputType IT.InputTime) (InputType IT.InputTime) = true
  eq (InputType IT.InputUrl) (InputType IT.InputUrl) = true
  eq (InputType IT.InputWeek) (InputType IT.InputWeek) = true
  eq _ _ = false


getStringAttribute :: Attribute -> Maybe (Tuple String String)
getStringAttribute = case _ of
  StringAttribute name val -> Just (Tuple name val)
  _ -> Nothing

getClass :: Attribute -> Maybe String
getClass = case _ of
  Class x -> Just x
  _ -> Nothing

getTextContent :: Attribute -> Maybe String
getTextContent = case _ of
  TextContent x -> Just x
  _ -> Nothing

getInputType :: Attribute -> Maybe InputType
getInputType = case _ of
  TypeInputType x -> Just x
  _ -> Nothing

getId :: Attribute -> Maybe String
getId = case _ of
  Id x -> Just x
  _ -> Nothing

instance showAttribute :: Show Attribute where
  show = case _ of
    Class s -> "(Class " <> s <> ")"
    Id s -> "(Id " <> s <> ")"
    TypeInputType s -> "(Type " <> (IT.renderInputType (unwrap s)) <> ")"
    StringAttribute a b -> "(StringAttribute " <> a <> " = " <> b <> ")"
    TextContent s -> "(TextContent " <> s <> ")"

-- | Split a list of attributes into a list of TextContent attributes,
-- | and everything else.
seperateTextContent :: List Attribute -> { tc :: List Attribute, other :: List Attribute }
seperateTextContent = foldl (\acc val -> case val of
    TextContent s -> acc { tc = snoc acc.tc val }
    _ -> acc { other = snoc acc.other val }
  ) { tc : mempty, other : mempty }

-- | Get the text content of an attribute
textContent :: Attribute -> Maybe String
textContent (TextContent s) = Just s
textContent _ = Nothing

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
