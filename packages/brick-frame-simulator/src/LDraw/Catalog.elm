module LDraw.Catalog exposing
    ( ElementCatalog
    , ElementInfo
    , PartIndex
    , PartInfo
    , bricklinkColorId
    , bricklinkPartId
    , cleanPartId
    , colorName
    , defaultPartWeight
    , elementCatalogDecoder
    , emptyCatalog
    , emptyPartIndex
    , estimateWeightFromDimensions
    , fromPartList
    , getElementId
    , getElementInfo
    , getPartInfo
    , insertElement
    , insertPart
    , partIndexDecoder
    )

{-| Part catalog and LEGO Element ID / BrickLink marketplace mapping table.

This module provides lookups from LDraw part numbers and color codes to:

  - Human-readable part descriptions and categories
  - Official LEGO Element IDs (e.g. 300121 for a Red 2x4 brick)
  - BrickLink part IDs and color IDs for Wanted List exports
  - Estimated weights and dimensions in LDU and grams

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


{-| Information about a specific LEGO element (part + color).
-}
type alias ElementInfo =
    { elementId : String
    , bricklinkPartId : String
    , bricklinkColorId : Int
    , colorName : String
    }


{-| Metadata for a part design independent of color.
-}
type alias PartInfo =
    { description : String
    , category : String
    , unitWeightGrams : Maybe Float
    , dimensionsLDU : Maybe ( Float, Float, Float )
    }


{-| In-memory lookup for part designs by normalized part ID.
-}
type alias PartIndex =
    Dict String PartInfo


{-| Mapping from `(normalizedPartId, ldrawColorId)` to element metadata.
-}
type alias ElementCatalog =
    Dict ( String, Int ) ElementInfo


{-| An empty element catalog.
-}
emptyCatalog : ElementCatalog
emptyCatalog =
    Dict.empty


{-| An empty part index.
-}
emptyPartIndex : PartIndex
emptyPartIndex =
    Dict.empty


{-| Normalize a part identifier: strip directory paths (`parts/`, `p/`),
file extensions (`.dat`, `.ldr`), and convert to lower case.

    cleanPartId "parts/3001.dat" == "3001"

    cleanPartId "3023.DAT" == "3023"

-}
cleanPartId : String -> String
cleanPartId raw =
    let
        withoutPath =
            case String.split "/" (String.replace "\\" "/" raw) |> List.reverse |> List.head of
                Just filename ->
                    filename

                Nothing ->
                    raw

        lower =
            String.toLower withoutPath
    in
    if String.endsWith ".dat" lower then
        String.dropRight 4 lower

    else if String.endsWith ".ldr" lower then
        String.dropRight 4 lower

    else
        lower


{-| BrickLink part identifier for an LDraw part.
-}
bricklinkPartId : String -> String
bricklinkPartId =
    cleanPartId


{-| Human-readable name for an LDraw color ID.
-}
colorName : Int -> String
colorName code =
    case code of
        0 ->
            "Black"

        1 ->
            "Blue"

        2 ->
            "Green"

        3 ->
            "Dark Turquoise"

        4 ->
            "Red"

        5 ->
            "Dark Pink"

        6 ->
            "Brown"

        7 ->
            "Light Gray"

        8 ->
            "Dark Gray"

        9 ->
            "Light Blue"

        10 ->
            "Bright Green"

        11 ->
            "Light Turquoise"

        12 ->
            "Salmon"

        13 ->
            "Pink"

        14 ->
            "Yellow"

        15 ->
            "White"

        16 ->
            "Current Color"

        17 ->
            "Light Green"

        18 ->
            "Light Yellow"

        19 ->
            "Tan"

        20 ->
            "Light Violet"

        21 ->
            "Glow In Dark Opaque"

        22 ->
            "Purple"

        23 ->
            "Dark Blue-Violet"

        24 ->
            "Edge Color"

        25 ->
            "Orange"

        26 ->
            "Magenta"

        27 ->
            "Lime"

        28 ->
            "Dark Tan"

        29 ->
            "Bright Pink"

        30 ->
            "Medium Lavender"

        31 ->
            "Lavender"

        32 ->
            "Trans-Black"

        33 ->
            "Trans-Dark Blue"

        34 ->
            "Trans-Green"

        35 ->
            "Trans-Bright Green"

        36 ->
            "Trans-Red"

        37 ->
            "Trans-Neon Orange"

        38 ->
            "Trans-Neon Yellow"

        39 ->
            "Trans-Very Light Blue"

        40 ->
            "Trans-Black"

        41 ->
            "Trans-Medium Blue"

        42 ->
            "Trans-Neon Green"

        43 ->
            "Trans-Light Blue"

        46 ->
            "Trans-Yellow"

        47 ->
            "Trans-Clear"

        68 ->
            "Very Light Orange"

        69 ->
            "Bright Purple"

        70 ->
            "Reddish Brown"

        71 ->
            "Light Bluish Gray"

        72 ->
            "Dark Bluish Gray"

        73 ->
            "Medium Blue"

        74 ->
            "Medium Green"

        77 ->
            "Light Pink"

        78 ->
            "Light Nougat"

        84 ->
            "Medium Dark Pink"

        85 ->
            "Dark Purple"

        89 ->
            "Royal Blue"

        92 ->
            "Nougat"

        100 ->
            "Light Salmon"

        110 ->
            "Ultra Light Orange"

        112 ->
            "Medium Violet"

        115 ->
            "Medium Lime"

        118 ->
            "Aqua"

        120 ->
            "Light Lime"

        125 ->
            "Light Orange"

        128 ->
            "Dark Nougat"

        134 ->
            "Copper"

        135 ->
            "Pearl Light Gray"

        137 ->
            "Metal Blue"

        142 ->
            "Pearl Gold"

        148 ->
            "Pearl Dark Gray"

        150 ->
            "Pearl Light Gold"

        151 ->
            "Very Light Bluish Gray"

        178 ->
            "Flat Dark Gold"

        179 ->
            "Flat Silver"

        191 ->
            "Bright Light Orange"

        212 ->
            "Bright Light Blue"

        226 ->
            "Bright Light Yellow"

        272 ->
            "Dark Blue"

        288 ->
            "Dark Green"

        308 ->
            "Dark Brown"

        313 ->
            "Maersk Blue"

        320 ->
            "Dark Red"

        321 ->
            "Dark Azure"

        322 ->
            "Medium Azure"

        323 ->
            "Light Aqua"

        326 ->
            "Yellowish Green"

        330 ->
            "Olive Green"

        335 ->
            "Sand Red"

        373 ->
            "Sand Purple"

        378 ->
            "Sand Green"

        379 ->
            "Sand Blue"

        383 ->
            "Chrome Silver"

        462 ->
            "Medium Orange"

        484 ->
            "Dark Orange"

        503 ->
            "Very Light Gray"

        _ ->
            "Color " ++ String.fromInt code


{-| Map an LDraw color ID to a BrickLink color ID.
-}
bricklinkColorId : Int -> Int
bricklinkColorId ldrawCode =
    case ldrawCode of
        0 ->
            11

        1 ->
            7

        2 ->
            6

        3 ->
            39

        4 ->
            5

        5 ->
            47

        6 ->
            8

        7 ->
            9

        8 ->
            10

        9 ->
            62

        10 ->
            36

        11 ->
            40

        12 ->
            25

        13 ->
            23

        14 ->
            3

        15 ->
            1

        17 ->
            38

        18 ->
            33

        19 ->
            2

        20 ->
            44

        21 ->
            46

        22 ->
            24

        25 ->
            4

        26 ->
            27

        27 ->
            34

        28 ->
            69

        29 ->
            104

        30 ->
            157

        31 ->
            154

        32 ->
            13

        33 ->
            14

        34 ->
            20

        35 ->
            108

        36 ->
            17

        37 ->
            18

        38 ->
            121

        39 ->
            113

        41 ->
            74

        42 ->
            16

        43 ->
            15

        46 ->
            19

        47 ->
            12

        68 ->
            96

        70 ->
            88

        71 ->
            86

        72 ->
            85

        73 ->
            42

        74 ->
            37

        77 ->
            56

        78 ->
            90

        84 ->
            94

        85 ->
            89

        89 ->
            97

        92 ->
            28

        110 ->
            96

        115 ->
            76

        118 ->
            41

        120 ->
            35

        125 ->
            32

        128 ->
            225

        134 ->
            84

        135 ->
            66

        137 ->
            78

        142 ->
            61

        148 ->
            77

        150 ->
            61

        151 ->
            99

        178 ->
            95

        179 ->
            95

        191 ->
            110

        212 ->
            105

        226 ->
            103

        272 ->
            63

        288 ->
            80

        308 ->
            120

        313 ->
            72

        320 ->
            59

        321 ->
            153

        322 ->
            156

        323 ->
            152

        326 ->
            158

        330 ->
            155

        335 ->
            58

        373 ->
            54

        378 ->
            48

        379 ->
            55

        383 ->
            21

        462 ->
            31

        484 ->
            68

        503 ->
            49

        _ ->
            ldrawCode


{-| Known weights for standard LEGO parts in grams.
-}
defaultPartWeight : String -> Maybe Float
defaultPartWeight rawPart =
    case cleanPartId rawPart of
        "3001" ->
            Just 2.32

        "3002" ->
            Just 1.74

        "3003" ->
            Just 1.16

        "3004" ->
            Just 0.58

        "3005" ->
            Just 0.29

        "3006" ->
            Just 5.8

        "3007" ->
            Just 4.64

        "3008" ->
            Just 2.32

        "3009" ->
            Just 1.74

        "3010" ->
            Just 1.16

        "3622" ->
            Just 0.87

        "3020" ->
            Just 0.77

        "3021" ->
            Just 0.58

        "3022" ->
            Just 0.39

        "3023" ->
            Just 0.19

        "3024" ->
            Just 0.1

        "3710" ->
            Just 0.39

        "3666" ->
            Just 0.58

        "3460" ->
            Just 0.77

        "3832" ->
            Just 1.93

        "3034" ->
            Just 1.54

        "3031" ->
            Just 1.54

        "3032" ->
            Just 2.32

        "3035" ->
            Just 3.09

        "3700" ->
            Just 0.65

        "3701" ->
            Just 1.3

        "3647" ->
            Just 0.4

        "3648" ->
            Just 1.8

        "3649" ->
            Just 3.5

        "6589" ->
            Just 0.6

        "32270" ->
            Just 0.8

        "32269" ->
            Just 1.8

        "3705" ->
            Just 0.5

        "3706" ->
            Just 0.75

        "3707" ->
            Just 1.0

        "2780" ->
            Just 0.15

        "3673" ->
            Just 0.14

        _ ->
            Nothing


{-| Estimate weight in grams from dimensions in LDU.
Uses standard ABS plastic density (1.05 g/cm³) and average LEGO hollow ratio (0.45).
1 LDU = 0.04 cm.
-}
estimateWeightFromDimensions : ( Float, Float, Float ) -> Float
estimateWeightFromDimensions ( dx, dy, dz ) =
    let
        cmX =
            max 1.0 (abs dx * 0.04)

        cmY =
            max 1.0 (abs dy * 0.04)

        cmZ =
            max 1.0 (abs dz * 0.04)

        volumeCm3 =
            cmX * cmY * cmZ

        densityGramsPerCm3 =
            1.05

        fillFactor =
            0.45

        calculated =
            volumeCm3 * densityGramsPerCm3 * fillFactor
    in
    max 0.1 calculated


{-| Look up part metadata by part ID.
-}
getPartInfo : String -> PartIndex -> Maybe PartInfo
getPartInfo rawPart index =
    Dict.get (cleanPartId rawPart) index


{-| Look up LEGO Element ID for a part and color.
-}
getElementId : String -> Int -> ElementCatalog -> Maybe String
getElementId rawPart color catalog =
    Dict.get ( cleanPartId rawPart, color ) catalog
        |> Maybe.map .elementId


{-| Look up full ElementInfo for a part and color.
-}
getElementInfo : String -> Int -> ElementCatalog -> Maybe ElementInfo
getElementInfo rawPart color catalog =
    Dict.get ( cleanPartId rawPart, color ) catalog


{-| Insert a part into the part index.
-}
insertPart : String -> PartInfo -> PartIndex -> PartIndex
insertPart rawPart info index =
    Dict.insert (cleanPartId rawPart) info index


{-| Insert an element into the catalog.
-}
insertElement : String -> Int -> ElementInfo -> ElementCatalog -> ElementCatalog
insertElement rawPart color info catalog =
    Dict.insert ( cleanPartId rawPart, color ) info catalog


{-| Build a PartIndex from a list of parts.
-}
fromPartList : List { file : String, description : String, category : String } -> PartIndex
fromPartList list =
    List.foldl
        (\item acc ->
            insertPart item.file
                { description = item.description
                , category = item.category
                , unitWeightGrams = defaultPartWeight item.file
                , dimensionsLDU = Nothing
                }
                acc
        )
        emptyPartIndex
        list


{-| Decoder for compact element catalog JSON payload.
Reads `{ "version": 1, "elements": { "3001:4": ["300121", "3001", 5, "Red"] } }`.
-}
elementCatalogDecoder : Decoder ElementCatalog
elementCatalogDecoder =
    let
        tupleDecoder =
            Decode.map4
                (\elemId blPart blColor colName ->
                    { elementId = elemId
                    , bricklinkPartId = blPart
                    , bricklinkColorId = blColor
                    , colorName = colName
                    }
                )
                (Decode.index 0 Decode.string)
                (Decode.index 1 Decode.string)
                (Decode.index 2 Decode.int)
                (Decode.index 3 Decode.string)

        parseKey key =
            case String.split ":" key of
                [ part, colorStr ] ->
                    case String.toInt colorStr of
                        Just c ->
                            Just ( cleanPartId part, c )

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        entriesDecoder =
            Decode.keyValuePairs tupleDecoder
                |> Decode.map
                    (\pairs ->
                        List.foldl
                            (\( k, v ) acc ->
                                case parseKey k of
                                    Just key ->
                                        Dict.insert key v acc

                                    Nothing ->
                                        acc
                            )
                            Dict.empty
                            pairs
                    )
    in
    Decode.field "version" Decode.int
        |> Decode.andThen
            (\version ->
                if version == 1 then
                    Decode.field "elements" entriesDecoder

                else
                    Decode.fail ("Unsupported element catalog version: " ++ String.fromInt version)
            )


{-| Decoder for part index JSON payload.
-}
partIndexDecoder : Decoder PartIndex
partIndexDecoder =
    let
        partTupleDecoder =
            Decode.map3
                (\file desc cat ->
                    ( cleanPartId file
                    , { description = desc
                      , category = cat
                      , unitWeightGrams = defaultPartWeight file
                      , dimensionsLDU = Nothing
                      }
                    )
                )
                (Decode.index 0 Decode.string)
                (Decode.index 1 Decode.string)
                (Decode.index 2 Decode.string)
    in
    Decode.field "version" Decode.int
        |> Decode.andThen
            (\version ->
                if version == 1 then
                    Decode.field "parts" (Decode.list partTupleDecoder)
                        |> Decode.map Dict.fromList

                else
                    Decode.fail ("Unsupported part index version: " ++ String.fromInt version)
            )
