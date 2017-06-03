module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Emmet.Halogen (emmetHalogen)
import Node.Encoding (Encoding(..))
import Node.Process (PROCESS, exit, stdin, stdout)
import Node.Stream (onDataString, writeString)

main ∷ ∀ e. Eff (console ∷ CONSOLE, exception ∷ EXCEPTION, process ∷ PROCESS, err ∷ EXCEPTION | e) Unit
main = do
  onDataString stdin UTF8 \input → do
    case emmetHalogen input of
      Right result →
        writeString stdout UTF8 result (exit 0) $> unit
      Left _ →
        writeString stdout UTF8 input (exit 1) $> unit
