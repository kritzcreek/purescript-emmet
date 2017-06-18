module Test.Main where

import Emmet
import Prelude

import Control.Monad.Aff (delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import DOM.HTML.Indexed.InputType (InputType(..)) as IT
import Data.Either (Either(..))
import Emmet.Halogen (emmetHalogen)
import Emmet.Parser as P
import Emmet.Types (InputType)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Text.Parsing.Parser (runParser)

main :: Eff (RunnerEffects ()) Unit
main = do
  log $ show $ emmetHalogen "input.class-name"
  log $ show $ emmetHalogen "input.class-name[type='text']"

  run [consoleReporter] do
    describe "Halogen Parser" do
      it "should parse type attributes on input elements" do
        let r1 = runParser "[type='text']" $ P.parseTypeInput "input"
        let r2 = runParser "[type=\"text\"]" $ P.parseTypeInput "input"
        let v = (Right $ TypeInputType (InputType (IT.InputText)))
        r1 `shouldEqual` v
        r2 `shouldEqual` v



    -- it "should generate halogen input and class" do
    --   let r1 = emmetHalogen "input.class-name]"
    --   r1 `shouldEqual` (Right "HH.input [HP.class $ ClassName \"class-name\"")
    -- it "should generate halogen input and type" do
    --   let r1 = emmetHalogen "input[type='text']"
    --   r1 `shouldEqual` (Right "HH.input [HP.type_ HP.InputText]")
