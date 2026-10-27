module Link exposing
    ( freeHash
    , freePayload
    , freeShareHash
    , freeSharePayload
    , puzzleHash
    , puzzlePayload
    )

{-| High-density binary link payloads for Brick Play builds.

  - Free building: encodes the whole board, glued placements included.
  - Puzzle: encodes the board and the counted kit.
  - Free share: keeps only the glued parts on the board and turns every loose
    part into a kit count, opening as a puzzle.

-}

import LDraw.Document as Document exposing (Document)
import Studio.HashPayload as HashPayload exposing (KitItem)


{-| The binary payload of free building: the whole board, glue included.
-}
freeHash : Document -> List Int
freeHash =
    freePayload


{-| The binary payload of free building: the whole board, glue included.
-}
freePayload : Document -> List Int
freePayload doc =
    HashPayload.encodeBytes
        { mode = HashPayload.FreeBuilding
        , document = doc
        , kit = Nothing
        }


{-| The binary payload of a puzzle: the board and the kit.
-}
puzzleHash : Document -> List KitItem -> List Int
puzzleHash =
    puzzlePayload


{-| The binary payload of a puzzle: the board and the kit.
-}
puzzlePayload : Document -> List KitItem -> List Int
puzzlePayload doc kit =
    HashPayload.encodeBytes
        { mode = HashPayload.Puzzle
        , document = doc
        , kit = Just kit
        }


{-| The puzzle a free build shares: its glued parts stay on the board, and every
other part becomes a kit count, grouped by file and colour.
-}
freeShareHash : Document -> List Int
freeShareHash =
    freeSharePayload


{-| The puzzle a free build shares: its glued parts stay on the board, and every
other part becomes a kit count, grouped by file and colour.
-}
freeSharePayload : Document -> List Int
freeSharePayload doc =
    let
        unglued =
            Document.placements doc
                |> List.filter (\placement -> not (Document.isGlued placement.id doc))

        gluedOnly =
            List.foldl (\placement acc -> Document.removePlacement placement.id acc) doc unglued
    in
    puzzlePayload gluedOnly (HashPayload.kitOfPlacements unglued)
