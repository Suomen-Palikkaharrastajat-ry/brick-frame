module Studio.UrlPayload exposing
    ( PartKitItem
    , Payload
    , buildPlayModelUrl
    , buildPlayUrl
    , decodeHash
    , decodeParts
    , encodeParts
    , encodePlayHash
    , encodePlayModelHash
    , extractKit
    , kitOfPlacements
    )

{-| URL hash encoding and decoding for exporting Studio models and part kits
to companion applications: Brick Play, with its kit alone or with a model.

Enables deep-linking between Palikkakehys applications with kit inventories
and model definitions passed directly through URL fragments.

-}

import Dict
import LDraw.Document as Document exposing (Document)
import LDraw.Writer as Writer
import Url


{-| A part kit item specifying a part file name, LDraw colour code, and count.
-}
type alias PartKitItem =
    { part : String
    , color : Int
    , count : Int
    }


{-| Decoded payload from a URL hash fragment.

`free` is set by `free=1` (Brick Play's free building mode). `partsListed` is set
when a `parts=` key is present at all, even an empty one, so an empty kit is kept
apart from no kit.

-}
type alias Payload =
    { parts : List PartKitItem
    , modelText : Maybe String
    , free : Bool
    , partsListed : Bool
    }


{-| Group document main placements by part file and colour into a kit inventory.
-}
extractKit : Document -> List PartKitItem
extractKit doc =
    kitOfPlacements (Document.mainPlacements doc)


{-| Group placements by part file and colour into a kit inventory, sorted by file
then colour, each with its count.
-}
kitOfPlacements : List Document.Placement -> List PartKitItem
kitOfPlacements placements =
    let
        counts =
            List.foldl
                (\placement acc ->
                    let
                        key =
                            placement.part ++ ":" ++ String.fromInt placement.color
                    in
                    Dict.update key
                        (\existing ->
                            case existing of
                                Just item ->
                                    Just { item | count = item.count + 1 }

                                Nothing ->
                                    Just { part = placement.part, color = placement.color, count = 1 }
                        )
                        acc
                )
                Dict.empty
                placements
    in
    counts
        |> Dict.values
        |> List.sortWith
            (\a b ->
                case compare a.part b.part of
                    EQ ->
                        compare a.color b.color

                    other ->
                        other
            )


{-| Encode a list of part kit items into a compact string representation.
Example: `"3001.dat:14:5,3002.dat:4:2"`.
-}
encodeParts : List PartKitItem -> String
encodeParts items =
    items
        |> List.map (\item -> item.part ++ ":" ++ String.fromInt item.color ++ ":" ++ String.fromInt item.count)
        |> String.join ","


{-| Decode a compact string representation of part kit items back to a list.
-}
decodeParts : String -> List PartKitItem
decodeParts str =
    if String.isEmpty (String.trim str) then
        []

    else
        str
            |> String.split ","
            |> List.filterMap
                (\chunk ->
                    case String.split ":" (String.trim chunk) of
                        [ part, colorStr, countStr ] ->
                            case ( String.toInt colorStr, String.toInt countStr ) of
                                ( Just color, Just count ) ->
                                    if String.isEmpty (String.trim part) || count <= 0 then
                                        Nothing

                                    else
                                        Just { part = String.trim part, color = color, count = count }

                                _ ->
                                    Nothing

                        _ ->
                            Nothing
                )


{-| Encode a document's kit into a Brick Play hash fragment string.
Produces `"parts=..."`.
-}
encodePlayHash : Document -> String
encodePlayHash doc =
    let
        partsStr =
            encodeParts (extractKit doc)
    in
    "parts=" ++ Url.percentEncode partsStr


{-| Encode a document's model and its kit into a Brick Play hash fragment string,
for a Technic mechanism (Brick Play turns Technic mode on by itself when the
model holds gears). Produces `"model=...&parts=..."`.
-}
encodePlayModelHash : Document -> String
encodePlayModelHash doc =
    let
        modelStr =
            Writer.writeDocument doc

        partsStr =
            encodeParts (extractKit doc)
    in
    "model=" ++ Url.percentEncode modelStr ++ "&parts=" ++ Url.percentEncode partsStr


{-| Parse a URL hash fragment (with or without leading `#`) into a Payload.
-}
decodeHash : String -> Payload
decodeHash rawHash =
    let
        withoutPrefix =
            if String.startsWith "#" rawHash then
                String.dropLeft 1 rawHash

            else
                rawHash

        entries =
            withoutPrefix
                |> String.split "&"
                |> List.filter (\chunk -> String.trim chunk /= "")
                |> List.filterMap
                    (\chunk ->
                        case String.split "=" chunk of
                            key :: rest ->
                                Just ( key, String.join "=" rest )

                            [] ->
                                Nothing
                    )
                |> Dict.fromList

        parts =
            Dict.get "parts" entries
                |> Maybe.map (\p -> Url.percentDecode p |> Maybe.withDefault p)
                |> Maybe.map decodeParts
                |> Maybe.withDefault []

        modelText =
            Dict.get "model" entries
                |> Maybe.map (\m -> Url.percentDecode m |> Maybe.withDefault m)
                |> Maybe.andThen
                    (\m ->
                        if String.isEmpty (String.trim m) then
                            Nothing

                        else
                            Just m
                    )
    in
    { parts = parts
    , modelText = modelText
    , free = Dict.get "free" entries == Just "1"
    , partsListed = Dict.member "parts" entries
    }


{-| Build a complete Brick Play URL from a base URL and a document.
-}
buildPlayUrl : String -> Document -> String
buildPlayUrl baseUrl doc =
    let
        hash =
            encodePlayHash doc

        cleanBase =
            if String.endsWith "#" baseUrl then
                String.dropRight 1 baseUrl

            else
                baseUrl
    in
    cleanBase ++ "#" ++ hash


{-| Build a complete Brick Play URL with the document's model and kit, from a base
URL and a document.
-}
buildPlayModelUrl : String -> Document -> String
buildPlayModelUrl baseUrl doc =
    let
        hash =
            encodePlayModelHash doc

        cleanBase =
            if String.endsWith "#" baseUrl then
                String.dropRight 1 baseUrl

            else
                baseUrl
    in
    cleanBase ++ "#" ++ hash
