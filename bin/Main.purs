module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Emmet.Halogen (emmetHalogen)
import Node.Encoding (Encoding(..))
import Node.Process (exit, stdin, stdout)
import Node.Stream (onDataString, writeString)

main ∷ Effect Unit
main = do
  onDataString stdin UTF8 \input → do
    case emmetHalogen input of
      Right result →
        writeString stdout UTF8 result (exit 0) $> unit
      Left _ →
        writeString stdout UTF8 input (exit 1) $> unit
