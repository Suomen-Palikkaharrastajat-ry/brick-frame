module Studio.PartIndex exposing
    ( Part
    , PartIndex
    , categories
    , decoder
    , fetch
    , makePart
    )

{-| The catalogue the part palette searches.

`scripts/build-part-index.mjs` writes it next to the synced LDraw library as
`parts-index.json`, one `[file, description, category, keywords?]` entry per part.
It is fetched once, when the palette is first opened, and searched in memory.

-}

import Dict
import Http
import Json.Decode as Decode exposing (Decoder)
import Studio.BuildGroups as BuildGroups exposing (Shelving)


{-| One part of the library. `haystack` is everything searchable about it, in
lower case, so a search is a substring test; `descriptionLower` is the
description alone, for ranking. `shelving` and `size` are the
palette's build groups and size order, worked out once here rather than on
every render.
-}
type alias Part =
    { file : String
    , description : String
    , category : String
    , haystack : String
    , descriptionLower : String
    , shelving : Shelving
    , size : List Float
    }


{-| A part from its index entry: file, description, category and keywords.
-}
makePart : String -> String -> String -> String -> Part
makePart file description category keywords =
    let
        entry =
            { file = file, description = description, category = category }
    in
    { file = file
    , description = description
    , category = category
    , haystack = String.toLower (String.join " " [ file, description, keywords ])
    , descriptionLower = String.toLower description
    , shelving = BuildGroups.classify entry
    , size = BuildGroups.sizeKey description
    }


{-| Every part, in file order, and the categories they fall into.
-}
type alias PartIndex =
    { parts : List Part
    , categories : List ( String, Int )
    }


{-| Read `{ "version": 1, "parts": [[file, description, category, keywords?], …] }`.
-}
decoder : Decoder PartIndex
decoder =
    Decode.field "version" Decode.int
        |> Decode.andThen
            (\version ->
                if version == 1 then
                    Decode.field "parts" (Decode.list partDecoder)
                        |> Decode.map (\parts -> { parts = parts, categories = categories parts })

                else
                    Decode.fail ("Unsupported part index version " ++ String.fromInt version)
            )


partDecoder : Decoder Part
partDecoder =
    Decode.map4 makePart
        (Decode.index 0 Decode.string)
        (Decode.index 1 Decode.string)
        (Decode.index 2 Decode.string)
        (Decode.oneOf [ Decode.index 3 Decode.string, Decode.succeed "" ])


{-| Fetch the index that sits in the LDraw library directory `ldrawBase`.
-}
fetch : String -> (Result Http.Error PartIndex -> msg) -> Cmd msg
fetch ldrawBase toMsg =
    Http.get
        { url = withoutTrailingSlash ldrawBase ++ "/parts-index.json"
        , expect = Http.expectJson toMsg decoder
        }


withoutTrailingSlash : String -> String
withoutTrailingSlash base =
    if String.endsWith "/" base then
        String.dropRight 1 base

    else
        base


{-| Categories with their part counts, most parts first.
-}
categories : List Part -> List ( String, Int )
categories parts =
    parts
        |> List.foldl
            (\part counts -> Dict.update part.category (\n -> Just (Maybe.withDefault 0 n + 1)) counts)
            Dict.empty
        |> Dict.toList
        |> List.sortBy (\( name, count ) -> ( -count, name ))
