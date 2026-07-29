module Lib (generateColorDataModule, generateElmModule) where

import Data.Bifunctor (second)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Read qualified as Text
import Numeric (showFFloat)
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

-- Colours are read from the official LDraw configuration file that ships with
-- the synced LDraw assets, rather than being maintained by hand here. That file
-- carries all 300+ official codes; a hand-kept subset silently rendered every
-- unlisted code as magenta.
ldConfigPath :: FilePath
ldConfigPath = "elm-app/public/ldraw/LDConfig.ldr"

-- Parse `0 !COLOUR <Name> CODE <n> VALUE #RRGGBB [EDGE ...] [ALPHA 0-255] ...`
-- lines. Keyword order after the name is not fixed, so each keyword is located
-- by scanning the token list rather than by position.
parseLdConfig :: FilePath -> IO [LDrawColor]
parseLdConfig path = do
    exists <- doesFileExist path
    if not exists
        then
            fail ("generator: LDraw colour config not found at " <> path)
        else do
            contents <- Text.readFile path
            let colors = mapMaybe parseColorLine (Text.lines contents)
            if null colors
                then
                    fail ("generator: no !COLOUR definitions found in " <> path)
                else
                    pure colors

parseColorLine :: Text -> Maybe LDrawColor
parseColorLine line =
    case Text.words line of
        ("0" : "!COLOUR" : name : rest) -> do
            code <- keywordValue "CODE" rest >>= readInt
            (r, g, b) <- keywordValue "VALUE" rest >>= parseHexRgb
            let alpha =
                    case keywordValue "ALPHA" rest >>= readInt of
                        Just a -> fromIntegral a / 255
                        Nothing -> 1.0
            Just (LDrawColor code name r g b alpha)
        _ ->
            Nothing

-- The token immediately following `keyword`, if present.
keywordValue :: Text -> [Text] -> Maybe Text
keywordValue keyword tokens =
    case dropWhile (/= keyword) tokens of
        (_ : value : _) -> Just value
        _ -> Nothing

readInt :: Text -> Maybe Int
readInt text =
    case Text.decimal text of
        Right (value, rest) | Text.null rest -> Just value
        _ -> Nothing

parseHexRgb :: Text -> Maybe (Float, Float, Float)
parseHexRgb value = do
    digits <- Text.stripPrefix "#" value
    if Text.length digits /= 6
        then
            Nothing
        else do
            r <- hexByte (Text.take 2 digits)
            g <- hexByte (Text.take 2 (Text.drop 2 digits))
            b <- hexByte (Text.take 2 (Text.drop 4 digits))
            pure (r, g, b)

hexByte :: Text -> Maybe Float
hexByte text =
    case Text.hexadecimal text of
        Right (value, rest) | Text.null rest -> Just (fromIntegral (value :: Int) / 255)
        _ -> Nothing

-- ── Gear data ─────────────────────────────────────────────────────────────────

data GearSpec = GearSpec
    { gearFile :: Text
    , gearTeeth :: Int
    , gearPitchRadius :: Float
    }
    deriving (Show)

-- Known gear parts with pitch radii in LDraw units (LDU).
-- Pitch radii: teeth × 1.25 LDU  (module M = 1 mm = 2.5 LDU, so r = teeth × M / 2)
knownGears :: [GearSpec]
knownGears =
    [ GearSpec "3647.dat" 8 10.0 -- 8T  (was 16.0)
    , GearSpec "10928.dat" 8 10.0 -- 8T  (was 16.0)
    , GearSpec "11955.dat" 8 10.0 -- 8T  (was 16.0)
    , GearSpec "4019.dat" 16 20.0 -- 16T (was 24.0)
    , GearSpec "3648.dat" 24 30.0 -- 24T (was 38.4)
    , GearSpec "3649.dat" 40 50.0 -- 40T (was 56.0)
    , GearSpec "4716.dat" 1 12.0 -- worm gear (pitch radius from 4716s01.dat geometry; thread root 7, tip 12.7; worm+12T sum ≈ 24 LDU)
    , GearSpec "32905.dat" 1 12.0 -- worm screw long Type II (same thread geometry as 4716)
    , GearSpec "69778.dat" 12 12.0 -- 12T gear (pitch radius from 69778s01.dat geometry; tooth root 12.3, tip 17.7; worm+12T sum ≈ 24 LDU)
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
    ldrawColors <- parseLdConfig ldConfigPath
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
            , "    " <> elmString "Palikkakehys"
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
            , "-- Known gear parts"
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

-- The simulator package renders through its own copy of the colour table, so it
-- gets the same generated data rather than a second hand-kept list that can
-- drift out of sync with Data.elm.
generateColorDataModule :: IO Text
generateColorDataModule = do
    ldrawColors <- parseLdConfig ldConfigPath
    pure $
        Text.unlines
            [ "module LDraw.ColorData exposing (colorTable)"
            , ""
            , "{-| LDraw colour code → RGBA, generated from " <> Text.pack ldConfigPath <> "."
            , ""
            , "Do not edit by hand — run `make generate`."
            , ""
            , "-}"
            , ""
            , "import Dict exposing (Dict)"
            , ""
            , ""
            , "{-| Every official LDraw colour, as linear RGBA in [0.0, 1.0]."
            , "-}"
            , "colorTable : Dict Int { r : Float, g : Float, b : Float, alpha : Float }"
            , "colorTable ="
            , "    Dict.fromList"
            , "        [ " <> Text.intercalate "\n        , " (map colorEntry ldrawColors)
            , "        ]"
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
    let s = showFFloat Nothing f ""
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
