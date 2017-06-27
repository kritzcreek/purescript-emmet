module Emmet.Types where

import Prelude

import Control.Monad.Free (Free, liftF)
import DOM.HTML.Indexed.InputType (InputType, InputType(..), renderInputType) as IT
import Data.Foldable (class Foldable, foldMap, foldl, length)
import Data.Functor.Mu (Mu)
import Data.Functor.Nu (Nu)
import Data.List (List, catMaybes, filter, singleton, snoc)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Matryoshka as M
import Matryoshka.Algebra (Algebra)
import Matryoshka.Coalgebra (GCoalgebra, Coalgebra)

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
  | StringAttribute String String
  | TextContent String

instance attributeEq :: Eq Attribute where
  eq (TypeInputType a) (TypeInputType b) = eq a b
  eq (Class a) (Class b) = eq a b
  eq (Id a) (Id b) = eq a b
  eq (StringAttribute a b) (StringAttribute c d) = (eq a c) && (eq b d)
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

data EmmetF a
  = Child a a
  | Sibling a a
  | ClimbUp a a
  | Multiplication a Int
  | Element String (List Attribute)
  | Text String

derive instance functorEmmetF :: Functor EmmetF

type Emmet = Nu EmmetF

element :: String -> List Attribute -> Emmet
element s as = M.embed (Element s as)

text :: String -> Emmet
text s = M.embed (Text s)

child :: Emmet -> Emmet -> Emmet
child a b = M.embed (Child a b)

climbUp :: Emmet -> Emmet -> Emmet
climbUp a b = M.embed (ClimbUp a b)

climbUpTransform :: Emmet -> Emmet
climbUpTransform e = M.futu climbUpAlgebra e

buildEmmet :: forall a. Algebra EmmetF Emmet
buildEmmet = case _ of
  (Child a b) -> M.embed $ (Child a b)
  (Sibling a b) -> M.embed (Sibling a b)
  (ClimbUp a b) -> M.embed (ClimbUp a b)
  (Multiplication a n) -> M.embed (Multiplication a n)
  (Element a b) -> M.embed (Element a b)
  (Text a) -> M.embed (Text a)

climbUpAlgebra :: GCoalgebra (Free EmmetF) EmmetF Emmet
climbUpAlgebra em = case M.project em of
  (Child a b) ->
    case (M.project b) of
      (ClimbUp c d) -> Sibling (liftF (Child a c)) (liftF $ M.project d)
      -- This could also work I think
      -- (ClimbUp c d) -> Child (liftF $ Sibling a d) (liftF $ M.project d)
      _ -> Child (liftF $ M.project a) (liftF $ M.project b)
  (Sibling a b) -> Sibling (liftF $ M.project a) (liftF $ M.project b)
  (Multiplication a n) -> Multiplication (liftF $ M.project a) n
  (Element a b) -> Element a b
  (ClimbUp a b) -> ClimbUp (liftF $ M.project a) (liftF $ M.project b)
  (Text a) -> Text a

seperateTextContent :: List Attribute -> { tc :: List Attribute, other :: List Attribute }
seperateTextContent = foldl (\acc val -> case val of
    TextContent s -> acc { tc = snoc acc.tc val }
    _ -> acc { other = snoc acc.other val }
  ) { tc : mempty, other : mempty }

textContent :: Attribute -> String
textContent (TextContent s) = s
textContent _ = ""

textContentTransform :: Emmet -> Emmet
textContentTransform e = M.futu textContentAlgebra e

textContentAlgebra :: GCoalgebra (Free EmmetF) EmmetF Emmet
textContentAlgebra em = case M.project em of
  (Child a b) -> Child (liftF $ M.project a) (liftF $ M.project b)
  (Sibling a b) -> Sibling (liftF $ M.project a) (liftF $ M.project b)
  (Multiplication a n) -> Multiplication (liftF $ M.project a) n
  (Element a b) ->
    let tc = seperateTextContent b
    in case length tc.tc of
      0 -> Element a tc.other
      _ -> Child (liftF $ (Element a tc.other)) (liftF $ Text $ foldMap id $ map textContent tc.tc)

  (ClimbUp a b) -> ClimbUp (liftF $ M.project a) (liftF $ M.project b)
  (Text a) -> Text a

transform :: Emmet -> Emmet
transform = textContentTransform >>> climbUpTransform

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
  ClimbUp a b -> "(ClimbUp " <> a <> " " <> b <> ")"
  Text s -> "(Text " <> s <> ")"
