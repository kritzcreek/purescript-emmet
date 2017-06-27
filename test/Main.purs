module Test.Main where

import Emmet
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM.HTML.Indexed.InputType (InputType(..)) as IT
import Data.Either (Either(..), isLeft)
import Data.List as List
import Data.Monoid (mempty)
import Emmet.Halogen (emmetHalogen)
import Emmet.Parser.Element as E
import Emmet.Parser.InputElement as IE
import Emmet.Types (Attribute(..), InputType, climbUp, element, ppEmmet, sibling, text, transform)
import Matryoshka.Class.Corecursive (embed)
import Matryoshka.Class.Recursive (project)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Text.Parsing.Parser (runParser)

parserEquals :: String -> Emmet -> Aff (RunnerEffects ()) Unit
parserEquals text alg =
  let v1 = ppEmmet alg
      v2 = ppEmmet <$> ((runParser <@> parseEmmet) text)
  in v2 `shouldEqual` (Right v1)

textTransformEquals :: String -> Emmet -> Aff (RunnerEffects ()) Unit
textTransformEquals text alg =
  let v1 = ppEmmet alg
      v2 = ppEmmet <$> textContentTransform <$> ((runParser <@> parseEmmet) text)
  in v2 `shouldEqual` (Right v1)

transformEquals :: String -> Emmet -> Aff (RunnerEffects ()) Unit
transformEquals text alg =
  let v1 = ppEmmet alg
      v2 = ppEmmet <$> transform <$> ((runParser <@> parseEmmet) text)
  in v2 `shouldEqual` (Right v1)

 -- log $ show $ ppEmmet <$> (textContentTransform <$> ((runParser <@> parseEmmet) "div>span{abc}"))

main :: Eff (RunnerEffects ()) Unit
main = do

  -- log $ show $ ppEmmet <$> (textContentTransform <$> ((runParser <@> parseEmmet) "div{abc}"))
  -- log $ show $ emmetHalogen "div{abc}"
  -- log $ show $ ppEmmet <$> (textContentTransform <$> ((runParser <@> parseEmmet) "div>span{abc}"))
  -- log $ show $ ppEmmet <$> (((runParser <@> parseEmmet) "div>span{abc}"))
  -- log $ show $ emmetHalogen "div>span{abc}"
  -- log $ show $ ppEmmet <$> (textContentTransform <$> ((runParser <@> parseEmmet) ))
  -- log $ show $ emmetHalogen "div.classname#id"

  run [consoleReporter] do
    describe "Parser - parseEmmet Parser" do
      it "Should parse div{abc}" do
        parserEquals
          "div{abc}"
          (element "div" $ List.singleton (TextContent "abc"))

      it "Should parse div>span{abc}" do
        parserEquals
          "div>span{abc}"
          ((child (element "div" mempty) (element "span" $ List.singleton (TextContent "abc"))))

      it "Should parse div>span{abc}>i" do
        parserEquals
          "div>span{abc}>i"
          (child (element "div" mempty) (child (element "span" $ List.singleton (TextContent "abc")) (element "i" mempty)))

      it "Should parse div>span{abc}>i.c" do
        parserEquals
          "div>span{abc}>i.c"
          (child (element "div" mempty) (child (element "span" $ List.singleton (TextContent "abc")) (element "i" $ List.singleton $ Class "c")))

      it "Should parse div>span{a}^input" do
        parserEquals
          "div>span{a}^input"
          (child (element "div" mempty) (climbUp (element "span" $ List.singleton (TextContent "a")) (element "input" mempty)))

      it "Should parse input[type='text']" do
        parserEquals
          "input[type='text']"
          (element "input" $ List.singleton (TypeInputType (InputType IT.InputText)))

      it "Should parse input.c[type='text']" do
        parserEquals
          "input.c[type='text']"
          (element "input" $ List.fromFoldable [Class "c", TypeInputType (InputType IT.InputText) ])

      it "should parse InputType type attributes on input elements only" do
        let r1 = runParser "[type='text']" $ IE.parseTypeInput "input"
        let r2 = runParser "[type=\"text\"]" $ IE.parseTypeInput "input"
        let r3 = runParser "div.nomatch" $ IE.parseInputElement
        let v = (Right $ TypeInputType (InputType (IT.InputText)))
        r1 `shouldEqual` v
        r2 `shouldEqual` v
        (isLeft r3) `shouldEqual` true

      it "Should parse input[placeholder='name']#id" do
        parserEquals
          "input[placeholder='name']#id"
          (element "input" $ List.fromFoldable [StringAttribute "placeholder" "name", Id "id"])

      it "Should parse div>{text}+{text}" do
        parserEquals
          "div>{text}+{text}"
          (child
            (element "div" mempty)
            (sibling
              (text "text")
              (text "text")
            )
          )

    describe "Tree Transformation - textContentTransform" do
      it "Should transform div{abc}" do
        textTransformEquals
          "div{abc}"
          (child (element "div" mempty) (text "abc"))

      it "Should transform div>span{abc}>i" do
        textTransformEquals
          "div>span{abc}>i"
          (child (element "div" mempty) (child (child (element "span" mempty) (text "abc")) (element "i" mempty)))

      it "Should transform div>span{abc}#id>i" do
        textTransformEquals
          "div>span{abc}#id>i"
          (child
            (element "div" mempty)
            (child
              (child
                (element "span" $ List.singleton (Id "id"))
                (text "abc")
              )
              (element "i" mempty)
            )
          )

    describe "Tree Transformation - combined transformation" do
      it "Should transform div>span{abc}#id^i" do
        transformEquals
          "div>span{abc}#id^i"
          (sibling
            (child
              (element "div" mempty)
              (child (element "span" $ List.singleton (Id "id")) (text "abc"))
            )
            (element "i" mempty)
          )
