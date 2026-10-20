module LDraw.BOM exposing
    ( BOMDiff
    , DiffLot
    , ModelBOM
    , PartLot
    , diffBOM
    , generateBOM
    , generateSubmodelBOM
    , toBrickLinkXml
    , toMarkdown
    , toPlainText
    , toRebrickableCsv
    )

{-| Bill of Materials (BOM) engine, aggregations, and marketplace export serializers.

This module provides:

  - Aggregation of part instances across submodels into part/color lots
  - Physical dimension calculation (LDU, studs, cm) from axis-aligned bounding boxes
  - Part mass and model weight estimation
  - BrickLink Wanted List XML serialization
  - Rebrickable CSV inventory serialization
  - Markdown table and plain text parts list formatters
  - Comparative revision diffing between model versions

-}

import Dict exposing (Dict)
import LDraw.Catalog as Catalog exposing (ElementCatalog, PartIndex)
import LDraw.Document as Document exposing (Document, Entry(..), Placement, Submodel)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set exposing (Set)


{-| A distinct lot of identical parts (same design ID and color).
-}
type alias PartLot =
    { partId : String
    , name : String
    , category : String
    , colorId : Int
    , colorName : String
    , elementId : Maybe String
    , count : Int
    , unitWeightGrams : Float
    , totalWeightGrams : Float
    , placementIds : List Int
    }


{-| Complete Bill of Materials for a model or submodel.
-}
type alias ModelBOM =
    { totalParts : Int
    , uniqueLots : Int
    , estimatedWeightGrams : Float
    , dimensionsLDU : ( Float, Float, Float )
    , dimensionsStuds : ( Float, Float, Float )
    , lots : List PartLot
    }


{-| Comparison of a single lot between two revisions.
-}
type alias DiffLot =
    { partId : String
    , colorId : Int
    , name : String
    , colorName : String
    , oldCount : Int
    , newCount : Int
    , diff : Int
    }


{-| Result of comparing two Bill of Materials revisions.
-}
type alias BOMDiff =
    { addedLots : List DiffLot
    , removedLots : List DiffLot
    , changedLots : List DiffLot
    , unchangedLots : List DiffLot
    , netPartsDiff : Int
    }


{-| Internal leaf part instance located in world space.
-}
type alias LeafInstance =
    { partId : String
    , color : Int
    , id : Int
    , matrix : Mat4
    }



-- ── BOM Generation ────────────────────────────────────────────────────────────


{-| Generate a full Bill of Materials for the entire model document.
Traverses the main submodel and expands any referenced submodels recursively.
Any non-referenced submodels are also aggregated so no parts are omitted.
-}
generateBOM : Document -> PartIndex -> ElementCatalog -> ModelBOM
generateBOM doc index catalog =
    let
        instances =
            collectAllInstances doc
    in
    buildBOMFromInstances instances index catalog


{-| Generate a Bill of Materials for a specific named submodel in the document.
If the name is empty or matches the main submodel, it behaves like `generateBOM`.
-}
generateSubmodelBOM : String -> Document -> PartIndex -> ElementCatalog -> ModelBOM
generateSubmodelBOM submodelName doc index catalog =
    if String.trim submodelName == "" || submodelName == "all" then
        generateBOM doc index catalog

    else
        let
            cleanTarget =
                Catalog.cleanPartId submodelName

            submodelMap =
                buildSubmodelMap doc

            instances =
                case Dict.get cleanTarget submodelMap of
                    Just targetSubmodel ->
                        expandSubmodel submodelMap
                            Nothing
                            (Set.singleton cleanTarget)
                            Mat4.identity
                            16
                            targetSubmodel
                            { instances = [], visited = Set.singleton cleanTarget }
                            |> .instances
                            |> List.reverse

                    Nothing ->
                        collectAllInstances doc
        in
        buildBOMFromInstances instances index catalog


buildSubmodelMap : Document -> Dict String Submodel
buildSubmodelMap doc =
    doc.submodels
        |> List.map (\s -> ( Catalog.cleanPartId s.name, s ))
        |> Dict.fromList


type alias TraversalState =
    { instances : List LeafInstance
    , visited : Set String
    }


collectAllInstances : Document -> List LeafInstance
collectAllInstances doc =
    let
        submodelMap =
            buildSubmodelMap doc

        mainSub =
            Document.mainSubmodel doc

        mainSubId =
            Catalog.cleanPartId mainSub.name

        initialVisited =
            if mainSubId /= "" then
                Set.singleton mainSubId

            else
                Set.empty

        mainState =
            expandSubmodel submodelMap
                Nothing
                initialVisited
                Mat4.identity
                16
                mainSub
                { instances = [], visited = initialVisited }

        otherSubmodels =
            List.drop 1 doc.submodels

        -- Find any non-main submodels that were not referenced from the main submodel
        unvisited =
            otherSubmodels
                |> List.filter (\s -> not (Set.member (Catalog.cleanPartId s.name) mainState.visited))

        finalState =
            List.foldl
                (\s accState ->
                    let
                        subId =
                            Catalog.cleanPartId s.name

                        subPath =
                            if subId /= "" then
                                Set.singleton subId

                            else
                                Set.empty
                    in
                    expandSubmodel submodelMap
                        Nothing
                        subPath
                        Mat4.identity
                        16
                        s
                        { accState | visited = Set.union subPath accState.visited }
                )
                mainState
                unvisited
    in
    List.reverse finalState.instances


expandSubmodel :
    Dict String Submodel
    -> Maybe Int
    -> Set String
    -> Mat4
    -> Int
    -> Submodel
    -> TraversalState
    -> TraversalState
expandSubmodel submodelMap rootSourceId currentPath worldMat parentColor submodel state =
    List.foldl
        (\entry accState ->
            case entry of
                PlacementEntry p ->
                    expandPlacement submodelMap rootSourceId currentPath worldMat parentColor p accState

                Raw _ ->
                    accState
        )
        state
        submodel.entries


expandPlacement :
    Dict String Submodel
    -> Maybe Int
    -> Set String
    -> Mat4
    -> Int
    -> Placement
    -> TraversalState
    -> TraversalState
expandPlacement submodelMap rootSourceId currentPath worldMat parentColor placement state =
    let
        cleanName =
            Catalog.cleanPartId placement.part

        combinedMat =
            Mat4.mul worldMat placement.matrix

        childColor =
            if placement.color == 16 || placement.color == -1 then
                parentColor

            else
                placement.color

        effectiveSourceId =
            case rootSourceId of
                Just rootId ->
                    rootId

                Nothing ->
                    placement.id
    in
    if Set.member cleanName currentPath then
        -- Avoid recursive cycle along current ancestor path
        state

    else
        case Dict.get cleanName submodelMap of
            Just childSubmodel ->
                let
                    nextPath =
                        Set.insert cleanName currentPath

                    nextVisited =
                        Set.insert cleanName state.visited
                in
                expandSubmodel submodelMap
                    (Just effectiveSourceId)
                    nextPath
                    combinedMat
                    childColor
                    childSubmodel
                    { state | visited = nextVisited }

            Nothing ->
                let
                    leaf =
                        { partId = cleanName
                        , color = childColor
                        , id = effectiveSourceId
                        , matrix = combinedMat
                        }
                in
                { state | instances = leaf :: state.instances }


buildBOMFromInstances : List LeafInstance -> PartIndex -> ElementCatalog -> ModelBOM
buildBOMFromInstances instances index catalog =
    let
        groupedLots =
            groupLots instances index catalog

        totalParts =
            List.sum (List.map .count groupedLots)

        uniqueLots =
            List.length groupedLots

        estimatedWeightGrams =
            List.sum (List.map .totalWeightGrams groupedLots)

        dimensionsLDU =
            calculateBoundingBox instances index

        ( lduX, lduY, lduZ ) =
            dimensionsLDU

        dimensionsStuds =
            ( lduX / 20.0, lduY / 20.0, lduZ / 20.0 )
    in
    { totalParts = totalParts
    , uniqueLots = uniqueLots
    , estimatedWeightGrams = round2 estimatedWeightGrams
    , dimensionsLDU = ( round2 lduX, round2 lduY, round2 lduZ )
    , dimensionsStuds = ( round2 (lduX / 20.0), round2 (lduY / 20.0), round2 (lduZ / 20.0) )
    , lots = groupedLots
    }


groupLots : List LeafInstance -> PartIndex -> ElementCatalog -> List PartLot
groupLots instances index catalog =
    let
        step instance dict =
            let
                key =
                    ( instance.partId, instance.color )

                updateEntry maybeList =
                    case maybeList of
                        Just ids ->
                            Just (instance.id :: ids)

                        Nothing ->
                            Just [ instance.id ]
            in
            Dict.update key updateEntry dict

        groupedMap =
            List.foldl step Dict.empty instances

        toLot ( ( partId, colorId ), ids ) =
            let
                count =
                    List.length ids

                partInfo =
                    Catalog.getPartInfo partId index

                name =
                    case partInfo of
                        Just info ->
                            info.description

                        Nothing ->
                            "Part " ++ partId

                category =
                    case partInfo of
                        Just info ->
                            info.category

                        Nothing ->
                            "Part"

                unitWeightGrams =
                    case partInfo |> Maybe.andThen .unitWeightGrams of
                        Just w ->
                            w

                        Nothing ->
                            case Catalog.defaultPartWeight partId of
                                Just w ->
                                    w

                                Nothing ->
                                    Catalog.estimateWeightFromDimensions ( 40.0, 24.0, 20.0 )

                totalWeightGrams =
                    round2 (unitWeightGrams * toFloat count)

                colName =
                    Catalog.colorName colorId

                elemId =
                    Catalog.getElementId partId colorId catalog

                uniqueIds =
                    List.foldl
                        (\id acc ->
                            if List.member id acc then
                                acc

                            else
                                id :: acc
                        )
                        []
                        (List.reverse ids)
                        |> List.reverse
            in
            { partId = partId
            , name = name
            , category = category
            , colorId = colorId
            , colorName = colName
            , elementId = elemId
            , count = count
            , unitWeightGrams = round2 unitWeightGrams
            , totalWeightGrams = totalWeightGrams
            , placementIds = uniqueIds
            }
    in
    Dict.toList groupedMap
        |> List.map toLot
        |> List.sortWith
            (\a b ->
                if a.count /= b.count then
                    compare b.count a.count

                else if a.partId /= b.partId then
                    compare a.partId b.partId

                else
                    compare a.colorId b.colorId
            )



-- ── Dimensions & Bounding Box ────────────────────────────────────────────────


calculateBoundingBox : List LeafInstance -> PartIndex -> ( Float, Float, Float )
calculateBoundingBox instances index =
    case instances of
        [] ->
            ( 0.0, 0.0, 0.0 )

        first :: rest ->
            let
                cornersOf inst =
                    let
                        halfExtents =
                            case Catalog.getPartInfo inst.partId index |> Maybe.andThen .dimensionsLDU of
                                Just ( dx, dy, dz ) ->
                                    ( dx / 2.0, dy / 2.0, dz / 2.0 )

                                Nothing ->
                                    ( 10.0, 12.0, 10.0 )

                        ( hx, hy, hz ) =
                            halfExtents

                        localCorners =
                            [ vec3 -hx -hy -hz
                            , vec3 hx -hy -hz
                            , vec3 -hx hy -hz
                            , vec3 hx hy -hz
                            , vec3 -hx -hy hz
                            , vec3 hx -hy hz
                            , vec3 -hx hy hz
                            , vec3 hx hy hz
                            ]
                    in
                    List.map (Mat4.transform inst.matrix) localCorners

                allPoints =
                    List.concatMap cornersOf (first :: rest)

                minMaxOf select =
                    let
                        values =
                            List.map select allPoints

                        minVal =
                            List.minimum values |> Maybe.withDefault 0.0

                        maxVal =
                            List.maximum values |> Maybe.withDefault 0.0
                    in
                    ( minVal, maxVal )

                ( minX, maxX ) =
                    minMaxOf Vec3.getX

                ( minY, maxY ) =
                    minMaxOf Vec3.getY

                ( minZ, maxZ ) =
                    minMaxOf Vec3.getZ
            in
            ( abs (maxX - minX), abs (maxY - minY), abs (maxZ - minZ) )



-- ── Marketplace Serializers ───────────────────────────────────────────────────


{-| Serialize a Bill of Materials to BrickLink Wanted List XML format.

    <INVENTORY>
      <ITEM>
        <ITEMTYPE>P</ITEMTYPE>
        <ITEMID>3001</ITEMID>
        <COLOR>5</COLOR>
        <MINQTY>12</MINQTY>
      </ITEM>
    </INVENTORY>

-}
toBrickLinkXml : ModelBOM -> String
toBrickLinkXml bom =
    let
        itemXml lot =
            let
                blPart =
                    escapeXml (Catalog.bricklinkPartId lot.partId)

                blColor =
                    Catalog.bricklinkColorId lot.colorId
            in
            String.join "\n"
                [ "  <ITEM>"
                , "    <ITEMTYPE>P</ITEMTYPE>"
                , "    <ITEMID>" ++ blPart ++ "</ITEMID>"
                , "    <COLOR>" ++ String.fromInt blColor ++ "</COLOR>"
                , "    <MINQTY>" ++ String.fromInt lot.count ++ "</MINQTY>"
                , "  </ITEM>"
                ]

        items =
            List.map itemXml bom.lots
    in
    String.join "\n" ([ "<INVENTORY>" ] ++ items ++ [ "</INVENTORY>", "" ])


{-| Serialize a Bill of Materials to Rebrickable CSV inventory format.

    Part,Color,Quantity,Is Alternate
    3001,4,12,False

-}
toRebrickableCsv : ModelBOM -> String
toRebrickableCsv bom =
    let
        header =
            "Part,Color,Quantity,Is Alternate"

        row lot =
            String.join ","
                [ escapeCsvField (Catalog.cleanPartId lot.partId)
                , String.fromInt lot.colorId
                , String.fromInt lot.count
                , "False"
                ]

        rows =
            List.map row bom.lots
    in
    String.join "\n" (header :: rows ++ [ "" ])


{-| Serialize a Bill of Materials to a GitHub-flavored Markdown parts table with summary.
-}
toMarkdown : ModelBOM -> String
toMarkdown bom =
    let
        ( cmX, cmY, cmZ ) =
            let
                ( lx, ly, lz ) =
                    bom.dimensionsLDU
            in
            ( round2 (lx * 0.04), round2 (ly * 0.04), round2 (lz * 0.04) )

        ( sx, sy, sz ) =
            bom.dimensionsStuds

        summaryLines =
            [ "# Bill of Materials"
            , ""
            , "- **Total Parts:** " ++ String.fromInt bom.totalParts
            , "- **Unique Lots:** " ++ String.fromInt bom.uniqueLots
            , "- **Estimated Weight:** " ++ String.fromFloat bom.estimatedWeightGrams ++ " g"
            , "- **Dimensions:** "
                ++ String.fromFloat cmX
                ++ " × "
                ++ String.fromFloat cmY
                ++ " × "
                ++ String.fromFloat cmZ
                ++ " cm ("
                ++ String.fromFloat sx
                ++ " × "
                ++ String.fromFloat sy
                ++ " × "
                ++ String.fromFloat sz
                ++ " studs)"
            , ""
            , "| Part | Name | Category | Color | Element ID | Qty | Unit Wt (g) | Total Wt (g) |"
            , "| :--- | :--- | :--- | :--- | :--- | ---: | ---: | ---: |"
            ]

        lotRow lot =
            let
                elemText =
                    Maybe.withDefault "-" lot.elementId

                colorText =
                    lot.colorName ++ " (" ++ String.fromInt lot.colorId ++ ")"
            in
            "| "
                ++ String.join " | "
                    [ escapeMarkdownCell lot.partId
                    , escapeMarkdownCell lot.name
                    , escapeMarkdownCell lot.category
                    , escapeMarkdownCell colorText
                    , escapeMarkdownCell elemText
                    , String.fromInt lot.count
                    , String.fromFloat lot.unitWeightGrams
                    , String.fromFloat lot.totalWeightGrams
                    ]
                ++ " |"

        tableRows =
            List.map lotRow bom.lots
    in
    String.join "\n" (summaryLines ++ tableRows ++ [ "" ])


{-| Serialize a Bill of Materials to a plain text parts list.
-}
toPlainText : ModelBOM -> String
toPlainText bom =
    let
        ( cmX, cmY, cmZ ) =
            let
                ( lx, ly, lz ) =
                    bom.dimensionsLDU
            in
            ( round2 (lx * 0.04), round2 (ly * 0.04), round2 (lz * 0.04) )

        ( sx, sy, sz ) =
            bom.dimensionsStuds

        headerLines =
            [ "Bill of Materials"
            , "Total Parts: " ++ String.fromInt bom.totalParts ++ ", Unique Lots: " ++ String.fromInt bom.uniqueLots
            , "Estimated Weight: " ++ String.fromFloat bom.estimatedWeightGrams ++ "g"
            , "Dimensions: "
                ++ String.fromFloat cmX
                ++ " x "
                ++ String.fromFloat cmY
                ++ " x "
                ++ String.fromFloat cmZ
                ++ " cm ("
                ++ String.fromFloat sx
                ++ " x "
                ++ String.fromFloat sy
                ++ " x "
                ++ String.fromFloat sz
                ++ " studs)"
            , ""
            ]

        lotLine lot =
            let
                elemText =
                    case lot.elementId of
                        Just el ->
                            " (Element: " ++ el ++ ")"

                        Nothing ->
                            ""
            in
            "- "
                ++ String.fromInt lot.count
                ++ "x "
                ++ lot.partId
                ++ " "
                ++ lot.name
                ++ " - "
                ++ lot.colorName
                ++ elemText
                ++ " ["
                ++ String.fromFloat lot.totalWeightGrams
                ++ "g]"

        lotLines =
            List.map lotLine bom.lots
    in
    String.join "\n" (headerLines ++ lotLines ++ [ "" ])



-- ── Revision Diffing ──────────────────────────────────────────────────────────


{-| Compare an old Bill of Materials against a new Bill of Materials.
Identifies added lots, removed lots, count adjustments, and net part change.
-}
diffBOM : ModelBOM -> ModelBOM -> BOMDiff
diffBOM oldBom newBom =
    let
        toMap bom =
            List.foldl
                (\lot acc -> Dict.insert ( lot.partId, lot.colorId ) lot acc)
                Dict.empty
                bom.lots

        oldMap =
            toMap oldBom

        newMap =
            toMap newBom

        allKeys =
            Set.toList (Set.union (Set.fromList (Dict.keys oldMap)) (Set.fromList (Dict.keys newMap)))

        categorize key acc =
            case ( Dict.get key oldMap, Dict.get key newMap ) of
                ( Nothing, Just nLot ) ->
                    let
                        dLot =
                            { partId = nLot.partId
                            , colorId = nLot.colorId
                            , name = nLot.name
                            , colorName = nLot.colorName
                            , oldCount = 0
                            , newCount = nLot.count
                            , diff = nLot.count
                            }
                    in
                    { acc | added = dLot :: acc.added }

                ( Just oLot, Nothing ) ->
                    let
                        dLot =
                            { partId = oLot.partId
                            , colorId = oLot.colorId
                            , name = oLot.name
                            , colorName = oLot.colorName
                            , oldCount = oLot.count
                            , newCount = 0
                            , diff = -oLot.count
                            }
                    in
                    { acc | removed = dLot :: acc.removed }

                ( Just oLot, Just nLot ) ->
                    let
                        delta =
                            nLot.count - oLot.count

                        dLot =
                            { partId = nLot.partId
                            , colorId = nLot.colorId
                            , name = nLot.name
                            , colorName = nLot.colorName
                            , oldCount = oLot.count
                            , newCount = nLot.count
                            , diff = delta
                            }
                    in
                    if delta == 0 then
                        { acc | unchanged = dLot :: acc.unchanged }

                    else
                        { acc | changed = dLot :: acc.changed }

                ( Nothing, Nothing ) ->
                    acc

        folded =
            List.foldl categorize { added = [], removed = [], changed = [], unchanged = [] } allKeys
    in
    { addedLots = List.reverse folded.added
    , removedLots = List.reverse folded.removed
    , changedLots = List.reverse folded.changed
    , unchangedLots = List.reverse folded.unchanged
    , netPartsDiff = newBom.totalParts - oldBom.totalParts
    }



-- ── Helpers ───────────────────────────────────────────────────────────────────


escapeXml : String -> String
escapeXml text =
    text
        |> String.replace "&" "&amp;"
        |> String.replace "<" "&lt;"
        |> String.replace ">" "&gt;"
        |> String.replace "\"" "&quot;"
        |> String.replace "'" "&apos;"


escapeCsvField : String -> String
escapeCsvField text =
    if String.contains "," text || String.contains "\"" text || String.contains "\n" text || String.contains "\u{000D}" text then
        "\"" ++ String.replace "\"" "\"\"" text ++ "\""

    else
        text


escapeMarkdownCell : String -> String
escapeMarkdownCell text =
    text
        |> String.replace "|" "\\|"
        |> String.replace "\u{000D}\n" " "
        |> String.replace "\n" " "


round2 : Float -> Float
round2 value =
    toFloat (round (value * 100.0)) / 100.0
