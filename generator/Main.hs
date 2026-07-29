module Main (main) where

import Data.Text.IO qualified as Text
import Lib (generateColorDataModule, generateElmModule)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
    createDirectoryIfMissing True "elm-app/src"
    elmModule <- generateElmModule
    Text.writeFile "elm-app/src/Data.elm" elmModule
    putStrLn "generator: wrote elm-app/src/Data.elm"

    createDirectoryIfMissing True "packages/brick-frame-simulator/src/LDraw"
    colorDataModule <- generateColorDataModule
    Text.writeFile "packages/brick-frame-simulator/src/LDraw/ColorData.elm" colorDataModule
    putStrLn "generator: wrote packages/brick-frame-simulator/src/LDraw/ColorData.elm"
