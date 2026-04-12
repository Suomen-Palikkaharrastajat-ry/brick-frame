module Main (main) where

import qualified Data.Text as Text
import Lib (generateElmModule)
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "generateElmModule" $ do
        it "produces the generated Elm module" $
            generateElmModule `shouldSatisfy` Text.isInfixOf "module Data exposing"
