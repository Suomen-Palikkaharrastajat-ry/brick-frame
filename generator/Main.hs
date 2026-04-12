module Main (main) where

import Data.Text.IO qualified as Text
import Lib (generateElmModule)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
    createDirectoryIfMissing True "elm-app/src"
    elmModule <- generateElmModule
    Text.writeFile "elm-app/src/Data.elm" elmModule
    putStrLn "generator: wrote elm-app/src/Data.elm"
