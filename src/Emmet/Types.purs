module Emmet.Types where

import Prelude

import Data.Functor.Nu (Nu)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Matryoshka as M

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
