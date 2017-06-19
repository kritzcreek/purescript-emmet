module Test.Main where

import Emmet
import Prelude

import Control.Monad.Aff (delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import DOM.HTML.Indexed.InputType (InputType(..)) as IT
import Data.Either (Either(..))
import Emmet.Halogen (emmetHalogen)
import Emmet.Parser.InputElement as IE
import Emmet.Parser.Element as E
import Emmet.Types (InputType)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Text.Parsing.Parser (runParser)

main :: Eff (RunnerEffects ()) Unit
main = do

  log $ show $ emmetHalogen "input.class-name"
  log $ show $ emmetHalogen "input.class-name[type='text'][placeholder='type here']"
  log $ show $ emmetHalogen "div.classname#id"

  run [consoleReporter] do
    describe "Halogen Parser - InputElement" do
      it "should parse type attributes on input elements" do
        let r1 = runParser "[type='text']" $ IE.parseTypeInput "input"
        let r2 = runParser "[type=\"text\"]" $ IE.parseTypeInput "input"
        let v = (Right $ TypeInputType (InputType (IT.InputText)))
        r1 `shouldEqual` v
        r2 `shouldEqual` v
      pending "should parse input[type='text']"
      pending "should parse input.classname[type='text']"
    describe "Halogen Parser - String Attributes" do
      it "should parse attributes that accept strings" do
        let r1 = runParser "[placeholder='some value']" $ E.parseGeneralStringAttribute
        let r2 = runParser """[placeholder="some value"]""" $ E.parseGeneralStringAttribute
        let v = (Right $ StringAttribute "placeholder" "some value")
        r1 `shouldEqual` v
        r2 `shouldEqual` v
