module Emmet
  ( module Types
  , module Parser
  , module Eval
  ) where

import Emmet.Types as Types
import Emmet.Parser as Parser
import Emmet.Eval as Eval


-- REPL helpers
{-
p ∷ String → Either ParseError String
p s = map Emmet.ppEmmet (runParser s Emmet.parseEmmet)

h ∷ String → String
h s =
  either show id
    (map (intercalate "\n" <<< map Emmet.renderHtmlBuilder)
     (map Emmet.evalEmmet (runParser s Emmet.parseEmmet)))
-}
