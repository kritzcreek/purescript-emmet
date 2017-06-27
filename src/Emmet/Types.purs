module Emmet.Types where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Foldable (foldMap, length)
import Data.Functor.Nu (Nu)
import Data.List (List, catMaybes)
import Matryoshka as M
import Matryoshka.Coalgebra (GCoalgebra)
import Emmet.Attribute (Attribute, seperateTextContent, textContent)

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
      _ -> Child (liftF $ (Element a tc.other)) (liftF $ Text $ foldMap id $ catMaybes $ map textContent tc.tc)

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
