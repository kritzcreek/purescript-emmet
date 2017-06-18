module Emmet.Types where

import Prelude

import DOM.HTML.Indexed.InputType (InputType, InputType(..), renderInputType) as IT
import Data.Functor.Nu (Nu)
import Data.Generic (class Generic, gEq)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Matryoshka as M

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

data Attribute
  = Class String
  | Id String
  | TypeInputType InputType

instance attributeEq :: Eq Attribute where
  eq (TypeInputType a) (TypeInputType b) = eq a b
  eq (Class a) (Class b) = eq a b
  eq (Id a) (Id b) = eq a b
  eq _ _ = false

getClass :: Attribute -> Maybe String
getClass = case _ of
  Class x -> Just x
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

data EmmetF a
  = Child a a
  | Sibling a a
  | Multiplication a Int
  | Element String (List Attribute)

derive instance functorEmmetF :: Functor EmmetF

type Emmet = Nu EmmetF

element :: String -> List Attribute -> Emmet
element s as = M.embed (Element s as)

child :: Emmet -> Emmet -> Emmet
child a b = M.embed (Child a b)

sibling :: Emmet -> Emmet -> Emmet
sibling a b = M.embed (Sibling a b)

multiplication :: Emmet -> Int -> Emmet
multiplication a b = M.embed (Multiplication a b)

ppEmmet :: Emmet -> String
ppEmmet = M.cata case _ of
  Child a b -> "(Child " <> a <> " " <> b <> ")"
  Sibling a b -> "(Sibling " <> a <> " " <> b <> ")"
  Multiplication a n -> "(Multiplication " <> a <> " " <> show n <> ")"
  Element name attrs -> "(Element " <> name <> " " <> show attrs <> ")"
