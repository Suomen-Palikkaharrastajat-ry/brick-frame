module Main (main) where

import qualified Data.Text as Text
import Lib (Finish (..), LDrawColor (..), generateColorDataModule, generateElmModule, parseLdConfig)
import Test.Hspec

ldConfigPath :: FilePath
ldConfigPath = "viewer-app/public/ldraw/LDConfig.ldr"

-- The finish the LDConfig line for `code` declares.
finishOfCode :: [LDrawColor] -> Int -> Maybe Finish
finishOfCode colors code =
    case filter ((== code) . colorCode) colors of
        (c : _) -> Just (colorFinish c)
        [] -> Nothing

main :: IO ()
main = hspec $ do
    describe "generateElmModule" $ do
        it "produces the generated Elm module" $ do
            generated <- generateElmModule
            generated `shouldSatisfy` Text.isInfixOf "module Data exposing"

    describe "generateColorDataModule" $ do
        it "produces the generated colour table module" $ do
            generated <- generateColorDataModule
            generated `shouldSatisfy` Text.isInfixOf "module LDraw.ColorData exposing"
            generated `shouldSatisfy` Text.isInfixOf "colorTable"

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

        it "emits a finish per colour code and the finishFor lookup" $ do
            generated <- generateColorDataModule
            generated `shouldSatisfy` Text.isInfixOf "finishFor : Int -> Finish"
            generated `shouldSatisfy` Text.isInfixOf "( 383, Chrome )"
            generated `shouldSatisfy` Text.isInfixOf "( 256, Rubber )"

    describe "colour finishes from LDConfig" $ do
        colors <- runIO (parseLdConfig ldConfigPath)

        it "gives a chrome code the Chrome finish" $
            finishOfCode colors 383 `shouldBe` Just Chrome

        it "gives a rubber code the Rubber finish" $
            finishOfCode colors 256 `shouldBe` Just Rubber

        it "gives a pearlescent code the Pearl finish" $
            finishOfCode colors 183 `shouldBe` Just Pearl

        it "gives a metallic code the Metal finish" $
            finishOfCode colors 80 `shouldBe` Just Metal

        it "gives an ALPHA-below-255 code the Transparent finish" $
            finishOfCode colors 34 `shouldBe` Just Transparent

        it "gives a glitter code the Glitter finish, even when translucent" $
            finishOfCode colors 117 `shouldBe` Just Glitter

        it "gives a speckle code the Speckle finish" $
            finishOfCode colors 75 `shouldBe` Just Speckle

        it "gives a plain code the Solid finish" $
            finishOfCode colors 4 `shouldBe` Just Solid
