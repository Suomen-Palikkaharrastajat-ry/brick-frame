module Lib (generateElmModule) where

import Data.Bifunctor (second)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- ── LDraw color table ─────────────────────────────────────────────────────────

data LDrawColor = LDrawColor
    { colorCode :: Int
    , colorName :: Text
    , colorR :: Float
    , colorG :: Float
    , colorB :: Float
    , colorAlpha :: Float
    }
    deriving (Show)

-- Standard LDraw color definitions.
-- Source: https://www.ldraw.org/article/547.html
ldrawColors :: [LDrawColor]
ldrawColors =
    [ LDrawColor 0 "Black" 0.067 0.067 0.067 1.0
    , LDrawColor 1 "Blue" 0.000 0.333 0.749 1.0
    , LDrawColor 2 "Green" 0.145 0.478 0.243 1.0
    , LDrawColor 3 "Dark Turquoise" 0.000 0.514 0.561 1.0
    , LDrawColor 4 "Red" 0.788 0.102 0.035 1.0
    , LDrawColor 5 "Dark Pink" 0.784 0.184 0.439 1.0
    , LDrawColor 6 "Brown" 0.357 0.212 0.067 1.0
    , LDrawColor 7 "Light Grey" 0.608 0.631 0.608 1.0
    , LDrawColor 8 "Dark Grey" 0.392 0.373 0.353 1.0
    , LDrawColor 9 "Light Blue" 0.682 0.831 0.933 1.0
    , LDrawColor 10 "Bright Green" 0.294 0.780 0.231 1.0
    , LDrawColor 11 "Light Turquoise" 0.000 0.659 0.682 1.0
    , LDrawColor 12 "Salmon" 0.988 0.565 0.478 1.0
    , LDrawColor 13 "Pink" 0.988 0.671 0.749 1.0
    , LDrawColor 14 "Yellow" 0.988 0.812 0.027 1.0
    , LDrawColor 15 "White" 1.000 1.000 1.000 1.0
    , LDrawColor 17 "Light Green" 0.710 0.902 0.710 1.0
    , LDrawColor 18 "Light Yellow" 0.988 0.929 0.624 1.0
    , LDrawColor 19 "Tan" 0.902 0.835 0.612 1.0
    , LDrawColor 20 "Light Violet" 0.812 0.729 0.878 1.0
    , LDrawColor 22 "Purple" 0.373 0.082 0.490 1.0
    , LDrawColor 23 "Dark Blue Violet" 0.122 0.141 0.620 1.0
    , LDrawColor 25 "Orange" 0.988 0.502 0.122 1.0
    , LDrawColor 26 "Magenta" 0.627 0.000 0.502 1.0
    , LDrawColor 27 "Lime" 0.749 0.878 0.118 1.0
    , LDrawColor 28 "Dark Tan" 0.639 0.537 0.337 1.0
    , LDrawColor 29 "Bright Pink" 0.988 0.671 0.780 1.0
    , LDrawColor 30 "Medium Lavender" 0.667 0.525 0.718 1.0
    , LDrawColor 31 "Lavender" 0.792 0.714 0.847 1.0
    , LDrawColor 36 "Very Light Orange" 0.988 0.769 0.518 1.0
    , LDrawColor 38 "Dark Orange" 0.651 0.251 0.047 1.0
    , LDrawColor 40 "Trans Black" 0.243 0.243 0.243 0.5
    , LDrawColor 41 "Trans Red" 0.902 0.071 0.008 0.5
    , LDrawColor 42 "Trans Neon Green" 0.773 0.988 0.000 0.5
    , LDrawColor 43 "Trans Light Blue" 0.537 0.851 0.988 0.5
    , LDrawColor 44 "Trans Light Purple" 0.682 0.525 0.741 0.5
    , LDrawColor 45 "Trans Dark Pink" 0.878 0.400 0.573 0.5
    , LDrawColor 46 "Trans Yellow" 0.988 0.855 0.165 0.5
    , LDrawColor 47 "Trans White" 0.878 0.878 0.878 0.5
    , LDrawColor 57 "Trans Orange" 0.988 0.502 0.122 0.5
    , LDrawColor 68 "Very Light Orange" 0.988 0.769 0.518 1.0
    , LDrawColor 69 "Bright Purple" 0.584 0.106 0.624 1.0
    , LDrawColor 70 "Reddish Brown" 0.408 0.188 0.067 1.0
    , LDrawColor 71 "Light Bluish Grey" 0.694 0.722 0.749 1.0
    , LDrawColor 72 "Dark Bluish Grey" 0.400 0.427 0.451 1.0
    , LDrawColor 73 "Medium Blue" 0.486 0.631 0.808 1.0
    , LDrawColor 74 "Medium Green" 0.467 0.733 0.396 1.0
    , LDrawColor 77 "Pink" 0.988 0.671 0.749 1.0
    , LDrawColor 78 "Light Flesh" 0.988 0.847 0.663 1.0
    , LDrawColor 84 "Medium Dark Flesh" 0.710 0.396 0.184 1.0
    , LDrawColor 85 "Dark Purple" 0.243 0.090 0.294 1.0
    , LDrawColor 86 "Dark Flesh" 0.580 0.286 0.114 1.0
    , LDrawColor 89 "Blue Violet" 0.251 0.341 0.659 1.0
    , LDrawColor 92 "Flesh" 0.851 0.549 0.302 1.0
    , LDrawColor 100 "Light Salmon" 0.988 0.729 0.639 1.0
    , LDrawColor 110 "Violet" 0.247 0.247 0.600 1.0
    , LDrawColor 112 "Medium Violet" 0.412 0.412 0.682 1.0
    , LDrawColor 115 "Medium Lime" 0.624 0.765 0.176 1.0
    , LDrawColor 118 "Aqua" 0.667 0.902 0.843 1.0
    , LDrawColor 120 "Light Lime" 0.812 0.902 0.494 1.0
    , LDrawColor 125 "Light Orange" 0.988 0.671 0.369 1.0
    , LDrawColor 128 "Dark Orange" 0.651 0.251 0.047 1.0
    , LDrawColor 151 "Very Light Bluish Grey" 0.859 0.875 0.878 1.0
    , LDrawColor 191 "Bright Light Orange" 0.988 0.671 0.047 1.0
    , LDrawColor 212 "Bright Light Blue" 0.624 0.812 0.988 1.0
    , LDrawColor 216 "Rust" 0.671 0.165 0.082 1.0
    , LDrawColor 226 "Bright Light Yellow" 0.988 0.929 0.447 1.0
    , LDrawColor 232 "Sky Blue" 0.533 0.753 0.878 1.0
    , LDrawColor 256 "Rubber Black" 0.067 0.067 0.067 1.0
    , LDrawColor 272 "Dark Blue" 0.000 0.173 0.475 1.0
    , LDrawColor 288 "Dark Green" 0.055 0.267 0.090 1.0
    , LDrawColor 308 "Dark Brown" 0.188 0.114 0.055 1.0
    , LDrawColor 320 "Dark Red" 0.486 0.000 0.027 1.0
    , LDrawColor 321 "Dark Azure" 0.000 0.549 0.773 1.0
    , LDrawColor 322 "Medium Azure" 0.341 0.710 0.867 1.0
    , LDrawColor 323 "Light Aqua" 0.788 0.945 0.886 1.0
    , LDrawColor 326 "Yellowish Green" 0.835 0.929 0.576 1.0
    , LDrawColor 329 "White Glow" 1.000 1.000 0.902 1.0
    , LDrawColor 334 "Pearl Gold" 0.769 0.608 0.224 1.0
    , LDrawColor 335 "Sand Red" 0.694 0.490 0.459 1.0
    , LDrawColor 366 "Earth Orange" 0.671 0.376 0.071 1.0
    , LDrawColor 373 "Sand Purple" 0.580 0.486 0.592 1.0
    , LDrawColor 378 "Sand Green" 0.482 0.608 0.518 1.0
    , LDrawColor 379 "Sand Blue" 0.427 0.518 0.616 1.0
    , LDrawColor 383 "Chrome Silver" 0.878 0.878 0.878 1.0
    , LDrawColor 462 "Light Orange" 0.988 0.671 0.369 1.0
    , LDrawColor 484 "Dark Orange" 0.651 0.251 0.047 1.0
    , LDrawColor 503 "Very Light Grey" 0.878 0.878 0.867 1.0
    ]

-- ── Gear data ─────────────────────────────────────────────────────────────────

data GearSpec = GearSpec
    { gearFile :: Text
    , gearTeeth :: Int
    , gearPitchRadius :: Float
    }
    deriving (Show)

-- Known Technic gear parts with pitch radii in LDraw units (LDU).
-- Pitch radii: teeth × 1.25 LDU  (module M = 1 mm = 2.5 LDU, so r = teeth × M / 2)
knownGears :: [GearSpec]
knownGears =
    [ GearSpec "3647.dat" 8 10.0 -- 8T  (was 16.0)
    , GearSpec "10928.dat" 8 10.0 -- 8T  (was 16.0)
    , GearSpec "11955.dat" 8 10.0 -- 8T  (was 16.0)
    , GearSpec "4019.dat" 16 20.0 -- 16T (was 24.0)
    , GearSpec "3648.dat" 24 30.0 -- 24T (was 38.4)
    , GearSpec "3649.dat" 40 50.0 -- 40T (was 56.0)
    , GearSpec "4716.dat" 1 6.0 -- worm gear (empirical; unchanged)
    , GearSpec "32198.dat" 20 25.0 -- bevel 20T (was 32.0)
    , GearSpec "32269.dat" 20 25.0 -- double bevel 20T (was 32.0)
    , GearSpec "3650b.dat" 24 30.0 -- crown gear (was 38.4)
    ]

knownExampleModels :: [(Text, Text)]
knownExampleModels =
    [ ("Car", "/ldraw/models/car.ldr")
    , ("Pyramid", "/ldraw/models/pyramid.ldr")
    , ("24T Gear", "/ldraw/parts/3648.dat")
    ]

-- ── Elm module generation ─────────────────────────────────────────────────────

generateElmModule :: IO Text
generateElmModule = do
    embeddedParts <- collectEmbeddedParts
    let lodParts = map (second simplifyPartText) embeddedParts
    pure $
        Text.unlines
            [ "module Data exposing (embeddedParts, exampleModels, gearParts, ldrawColors, lodParts, message, version)"
            , ""
            , ""
            , "import Dict exposing (Dict)"
            , ""
            , ""
            , "message : String"
            , "message ="
            , "    " <> elmString "LEGO Technic Simulator"
            , ""
            , ""
            , "version : String"
            , "version ="
            , "    " <> elmString "0.1.0"
            , ""
            , ""
            , "-- LDraw color table: code -> { r, g, b, alpha } (values 0.0-1.0)"
            , "ldrawColors : Dict Int { r : Float, g : Float, b : Float, alpha : Float }"
            , "ldrawColors ="
            , "    Dict.fromList"
            , "        [ " <> Text.intercalate "\n        , " (map colorEntry ldrawColors)
            , "        ]"
            , ""
            , ""
            , "-- Embedded core gear parts and transitive dependencies"
            , "embeddedParts : Dict String String"
            , "embeddedParts ="
            , "    Dict.fromList"
            , "        [ " <> Text.intercalate "\n        , " (map embeddedPartEntry embeddedParts)
            , "        ]"
            , ""
            , ""
            , "-- Simplified LOD meshes for distant rendering"
            , "lodParts : Dict String String"
            , "lodParts ="
            , "    Dict.fromList"
            , "        [ " <> Text.intercalate "\n        , " (map embeddedPartEntry lodParts)
            , "        ]"
            , ""
            , ""
            , "-- Known Technic gear parts"
            , "gearParts : List { partFile : String, teeth : Int, pitchRadius : Float }"
            , "gearParts ="
            , "    [ " <> Text.intercalate "\n    , " (map gearEntry knownGears)
            , "    ]"
            , ""
            , ""
            , "-- Built-in model presets for quick testing"
            , "exampleModels : List { label : String, url : String }"
            , "exampleModels ="
            , "    [ " <> Text.intercalate "\n    , " (map exampleModelEntry knownExampleModels)
            , "    ]"
            ]

colorEntry :: LDrawColor -> Text
colorEntry c =
    "( "
        <> Text.pack (show (colorCode c))
        <> ", { r = "
        <> showF (colorR c)
        <> ", g = "
        <> showF (colorG c)
        <> ", b = "
        <> showF (colorB c)
        <> ", alpha = "
        <> showF (colorAlpha c)
        <> " } )"

gearEntry :: GearSpec -> Text
gearEntry g =
    "{ partFile = "
        <> elmString (gearFile g)
        <> ", teeth = "
        <> Text.pack (show (gearTeeth g))
        <> ", pitchRadius = "
        <> showF (gearPitchRadius g)
        <> " }"

exampleModelEntry :: (Text, Text) -> Text
exampleModelEntry (label, url) =
    "{ label = "
        <> elmString label
        <> ", url = "
        <> elmString url
        <> " }"

embeddedPartEntry :: (Text, Text) -> Text
embeddedPartEntry (name, content) =
    "( "
        <> elmString name
        <> ", "
        <> elmString content
        <> " )"

showF :: Float -> Text
showF f =
    let s = show f
     in if '.' `elem` s then Text.pack s else Text.pack s <> ".0"

elmString :: Text -> Text
elmString value =
    "\"" <> Text.concatMap escapeChar value <> "\""

escapeChar :: Char -> Text
escapeChar char =
    case char of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ -> Text.singleton char

embeddedRootParts :: [Text]
embeddedRootParts =
    [ "3647.dat"
    , "4019.dat"
    , "3648.dat"
    , "3649.dat"
    ]

collectEmbeddedParts :: IO [(Text, Text)]
collectEmbeddedParts =
    go Set.empty embeddedRootParts []
  where
    go :: Set Text -> [Text] -> [(Text, Text)] -> IO [(Text, Text)]
    go _ [] acc =
        pure (reverse acc)
    go visited (name : rest) acc
        | Set.member name visited =
            go visited rest acc
        | otherwise = do
            maybeContent <- readPartFile name
            let nextVisited = Set.insert name visited
            case maybeContent of
                Nothing ->
                    go nextVisited rest acc
                Just content ->
                    let deps = extractSubfileRefs content
                     in go nextVisited (deps ++ rest) ((name, content) : acc)

readPartFile :: Text -> IO (Maybe Text)
readPartFile partName = do
    let namePath = Text.unpack partName
    let candidates
            | "s/" `Text.isPrefixOf` partName =
                ["elm-app/public/ldraw/parts" </> namePath]
            | "48/" `Text.isPrefixOf` partName =
                ["elm-app/public/ldraw/p" </> namePath]
            | otherwise =
                [ "elm-app/public/ldraw/parts" </> namePath
                , "elm-app/public/ldraw/p" </> namePath
                ]
    existing <- firstExistingFile candidates
    case existing of
        Just path ->
            Just <$> Text.readFile path
        Nothing ->
            pure Nothing

firstExistingFile :: [FilePath] -> IO (Maybe FilePath)
firstExistingFile [] =
    pure Nothing
firstExistingFile (path : rest) = do
    exists <- doesFileExist path
    if exists
        then
            pure (Just path)
        else
            firstExistingFile rest

extractSubfileRefs :: Text -> [Text]
extractSubfileRefs content =
    mapMaybe subfileRefFromLine (Text.lines content)

subfileRefFromLine :: Text -> Maybe Text
subfileRefFromLine line =
    let tokens = Text.words line
     in case tokens of
            (lineType : _) | lineType /= "1" -> Nothing
            _ ->
                if length tokens >= 15
                    then
                        Just (normaliseName (last tokens))
                    else
                        Nothing

normaliseName :: Text -> Text
normaliseName =
    Text.toLower
        . Text.replace "\\" "/"
        . Text.strip

simplifyPartText :: Text -> Text
simplifyPartText content =
    let
        (_, kept) =
            foldl
                ( \(geomIx, acc) line ->
                    let
                        tokens =
                            Text.words line

                        maybeType =
                            case tokens of
                                t : _ ->
                                    Just t
                                [] ->
                                    Nothing
                     in
                        case maybeType of
                            Just "0" ->
                                (geomIx, line : acc)
                            Just "1" ->
                                -- Keep structural sub-file references intact.
                                (geomIx, line : acc)
                            Just "2" ->
                                if even geomIx
                                    then
                                        (geomIx + 1, line : acc)
                                    else
                                        (geomIx + 1, acc)
                            Just "3" ->
                                if even geomIx
                                    then
                                        (geomIx + 1, line : acc)
                                    else
                                        (geomIx + 1, acc)
                            Just "4" ->
                                if even geomIx
                                    then
                                        (geomIx + 1, line : acc)
                                    else
                                        (geomIx + 1, acc)
                            Just "5" ->
                                if even geomIx
                                    then
                                        (geomIx + 1, line : acc)
                                    else
                                        (geomIx + 1, acc)
                            _ ->
                                (geomIx, line : acc)
                )
                (0, [])
                (Text.lines content)
     in
        Text.unlines (reverse kept)
