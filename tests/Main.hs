module Main (main) where

import qualified Data.Text as Text
import Lib (generateColorDataModule, generateElmModule)
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "generateElmModule" $ do
        it "produces the generated Elm module" $ do
            generated <- generateElmModule
            generated `shouldSatisfy` Text.isInfixOf "module Data exposing"

    describe "generateColorDataModule" $ do
        it "produces the generated colour table module" $ do
            generated <- generateColorDataModule
            generated `shouldSatisfy` Text.isInfixOf "module LDraw.ColorData exposing (colorTable)"

        it "covers the whole LDConfig catalog, not a hand-kept subset" $ do
            generated <- generateColorDataModule
            -- The old hand-maintained table held 92 entries.
            length (filter (Text.isInfixOf ", { r = ") (Text.lines generated))
                `shouldSatisfy` (> 300)

        it "includes codes that used to render as magenta" $ do
            generated <- generateColorDataModule
            -- 33/34/54/297 are used by real Studio models and were absent before.
            mapM_
                (\code -> generated `shouldSatisfy` Text.isInfixOf ("( " <> code <> ", { r = "))
                ["33", "34", "54", "297"]

        it "parses ALPHA into a fractional alpha channel" $ do
            generated <- generateColorDataModule
            -- Trans_Dark_Blue is ALPHA 128, i.e. 128/255.
            generated `shouldSatisfy` Text.isInfixOf "( 33, { r = 0.0, g = 0.1254902, b = 0.627451, alpha = 0.5019608 } )"
