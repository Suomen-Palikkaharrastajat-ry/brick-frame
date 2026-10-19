module LDraw.Document exposing
    ( Document
    , Entry(..)
    , Placement
    , Submodel
    , addPlacement
    , clearTouched
    , empty
    , entryToText
    , fingerprint
    , fromLines
    , getPlacement
    , isMultiFile
    , mainPlacements
    , mainSubmodel
    , parse
    , placements
    , removePlacement
    , subfiles
    , subfilesSignature
    , toLines
    , toLinesOf
    , updatePlacement
    )

{-| Editable, round-trippable LDraw document.

`LDraw.Parser` turns text into `LDrawLine` values and drops everything it does
not render (META lines, blank lines, Studio-specific tokens). A document keeps
all of it: every submodel is an ordered list of entries, where a part reference
is a structured `Placement` and every other line is kept verbatim as `Raw`.
`LDraw.Writer` is the reverse of `parse`.

`touched` collects the ids of placements added, changed or removed by the
editing functions since the last `clearTouched`, so a renderer can update just
those instead of comparing the whole document.

Placements get stable integer ids when the document is parsed (or when a
placement is added). Ids are never reused within a document, so selection,
undo history and instanced entities can all refer to a placement by id.

A file without `0 FILE` lines is a single submodel with the empty name; the
writer emits it as a plain `.ldr`. Anything with named submodels is an `.mpd`.

-}

import LDraw.Format as Format
import LDraw.Parser as Parser
import LDraw.Types exposing (LDrawLine(..), SubFileData)
import Math.Matrix4 exposing (Mat4)
import Set exposing (Set)


{-| A whole file: ordered submodels plus the next free placement id.
-}
type alias Document =
    { submodels : List Submodel
    , nextId : Int
    , touched : Set Int
    }


{-| One `0 FILE` section of an MPD, or the whole file of a plain LDR.
`name` is kept as written (the empty string for a plain LDR).
-}
type alias Submodel =
    { name : String
    , entries : List Entry
    }


{-| A line of a submodel.
-}
type Entry
    = PlacementEntry Placement
    | Raw String


{-| A type 1 (or Studio type 11) part reference.

  - `part` is the normalised file name used for lookups, `fileText` is the
    name exactly as written in the source.
  - `extra` holds the Studio type 11 tokens that sit between the colour and
    the position (`uid`, hidden flag, step). Empty for a plain type 1 line.

-}
type alias Placement =
    { id : Int
    , part : String
    , fileText : String
    , color : Int
    , matrix : Mat4
    , extra : List String
    }



-- ── Construction ──────────────────────────────────────────────────────────────


{-| A document with one empty, unnamed submodel.
-}
empty : Document
empty =
    { submodels = [ { name = "", entries = [] } ]
    , nextId = 0
    , touched = Set.empty
    }


{-| Parse `.ldr` / `.mpd` text, keeping every line.
-}
parse : String -> Document
parse text =
    let
        step line ( done, current, nextId ) =
            case fileHeader line of
                Just name ->
                    ( closeSubmodel current done, { name = name, entries = [] }, nextId )

                Nothing ->
                    case parsePlacement nextId line of
                        Just placement ->
                            ( done
                            , { current | entries = PlacementEntry placement :: current.entries }
                            , nextId + 1
                            )

                        Nothing ->
                            ( done, { current | entries = Raw line :: current.entries }, nextId )

        ( closed, last, finalId ) =
            List.foldl step ( [], { name = "", entries = [] }, 0 ) (String.lines text)

        all =
            List.reverse (closeSubmodel last closed)

        -- Text before the first `0 FILE` is usually empty; drop that phantom
        -- submodel when real ones follow.
        submodels =
            case all of
                first :: rest ->
                    if first.name == "" && List.isEmpty first.entries && not (List.isEmpty rest) then
                        rest

                    else
                        all

                [] ->
                    empty.submodels
    in
    { submodels = submodels, nextId = finalId, touched = Set.empty }


{-| Build a single-submodel document from already parsed lines. Comments become
raw `0` lines; geometry lines are written back with their coordinates.
-}
fromLines : List LDrawLine -> Document
fromLines lines =
    let
        step line ( entries, nextId ) =
            case line of
                SubFileRef data ->
                    ( PlacementEntry (placementFromData nextId data) :: entries, nextId + 1 )

                other ->
                    ( Raw (Format.lineToText other) :: entries, nextId )

        ( reversed, finalId ) =
            List.foldl step ( [], 0 ) lines
    in
    { submodels = [ { name = "", entries = List.reverse reversed } ]
    , nextId = finalId
    , touched = Set.empty
    }



-- ── Queries ───────────────────────────────────────────────────────────────────


{-| True when the document has named submodels and so must be written as `.mpd`.
-}
isMultiFile : Document -> Bool
isMultiFile doc =
    case doc.submodels of
        [ only ] ->
            only.name /= ""

        _ ->
            True


{-| The first submodel — the model proper in an MPD.
-}
mainSubmodel : Document -> Submodel
mainSubmodel doc =
    doc.submodels
        |> List.head
        |> Maybe.withDefault { name = "", entries = [] }


{-| Every placement in the document, in file order.
-}
placements : Document -> List Placement
placements doc =
    doc.submodels
        |> List.concatMap .entries
        |> List.filterMap placementOf


{-| The placements of the main submodel, in file order.
-}
mainPlacements : Document -> List Placement
mainPlacements doc =
    (mainSubmodel doc).entries |> List.filterMap placementOf


{-| The submodels other than the main one, as parsed lines under their
normalised file names — what a part cache needs to resolve references to them.
-}
subfiles : Document -> List ( String, List LDrawLine )
subfiles doc =
    doc.submodels
        |> List.drop 1
        |> List.map (\s -> ( normalise s.name, entriesToLines s.entries ))


{-| Text of every non-main submodel. Equal signatures mean a part cache seeded
from `subfiles` is still valid.
-}
subfilesSignature : Document -> String
subfilesSignature doc =
    doc.submodels
        |> List.drop 1
        |> List.concatMap (\s -> s.name :: List.map entryToText s.entries)
        |> String.join "\n"


{-| What makes two placements draw the same: part, colour and position, but not
the id. Equal fingerprints mean the scene need not change.
-}
fingerprint : Placement -> String
fingerprint =
    Format.placementToText


{-| One entry as the line it is written as.
-}
entryToText : Entry -> String
entryToText entry =
    case entry of
        PlacementEntry p ->
            Format.placementToText p

        Raw text ->
            text


{-| Forget which placements changed; call once a renderer has caught up.
-}
clearTouched : Document -> Document
clearTouched doc =
    { doc | touched = Set.empty }


{-| Look a placement up by id.
-}
getPlacement : Int -> Document -> Maybe Placement
getPlacement id doc =
    placements doc
        |> List.filter (\p -> p.id == id)
        |> List.head


{-| The lines of the main submodel, in the form the geometry and gear code
consume.
-}
toLines : Document -> List LDrawLine
toLines doc =
    entriesToLines (mainSubmodel doc).entries


{-| Lines for one submodel, matched by normalised name.
-}
toLinesOf : String -> Document -> List LDrawLine
toLinesOf name doc =
    case List.filter (\s -> normalise s.name == normalise name) doc.submodels of
        found :: _ ->
            entriesToLines found.entries

        [] ->
            []



-- ── Editing ───────────────────────────────────────────────────────────────────


{-| Append a new placement to the named submodel (the main one when the name
is unknown). Returns the document and the id given to the new placement.
-}
addPlacement : String -> { part : String, color : Int, matrix : Mat4 } -> Document -> ( Document, Int )
addPlacement submodelName spec doc =
    let
        id =
            doc.nextId

        placement =
            { id = id
            , part = normalise spec.part
            , fileText = spec.part
            , color = spec.color
            , matrix = spec.matrix
            , extra = []
            }

        targetName =
            if List.any (\s -> s.name == submodelName) doc.submodels then
                submodelName

            else
                (mainSubmodel doc).name

        add submodel =
            if submodel.name == targetName then
                { submodel | entries = submodel.entries ++ [ PlacementEntry placement ] }

            else
                submodel
    in
    ( { doc
        | submodels = List.map add doc.submodels
        , nextId = id + 1
        , touched = Set.insert id doc.touched
      }
    , id
    )


{-| Change one placement. Unknown ids leave the document untouched.
-}
updatePlacement : Int -> (Placement -> Placement) -> Document -> Document
updatePlacement id change doc =
    let
        entry e =
            case e of
                PlacementEntry p ->
                    if p.id == id then
                        PlacementEntry (change p)

                    else
                        e

                Raw _ ->
                    e
    in
    mapEntries (List.map entry) { doc | touched = Set.insert id doc.touched }


{-| Delete one placement.
-}
removePlacement : Int -> Document -> Document
removePlacement id doc =
    let
        keep e =
            case e of
                PlacementEntry p ->
                    p.id /= id

                Raw _ ->
                    True
    in
    mapEntries (List.filter keep) { doc | touched = Set.insert id doc.touched }



-- ── Internals ─────────────────────────────────────────────────────────────────


mapEntries : (List Entry -> List Entry) -> Document -> Document
mapEntries f doc =
    { doc | submodels = List.map (\s -> { s | entries = f s.entries }) doc.submodels }


closeSubmodel : Submodel -> List Submodel -> List Submodel
closeSubmodel current acc =
    { current | entries = List.reverse current.entries } :: acc


placementOf : Entry -> Maybe Placement
placementOf entry =
    case entry of
        PlacementEntry p ->
            Just p

        Raw _ ->
            Nothing


entriesToLines : List Entry -> List LDrawLine
entriesToLines entries =
    entries
        |> List.filterMap
            (\entry ->
                case entry of
                    PlacementEntry p ->
                        Just (SubFileRef { color = p.color, transform = p.matrix, file = p.part })

                    Raw text ->
                        Parser.parseLine text
            )


{-| `0 FILE name` → the name as written.
-}
fileHeader : String -> Maybe String
fileHeader line =
    case String.words line of
        "0" :: "FILE" :: rest ->
            Just (String.join " " rest)

        _ ->
            Nothing


parsePlacement : Int -> String -> Maybe Placement
parsePlacement id line =
    case String.words line of
        "1" :: rest ->
            placementFromTokens id [] rest

        "11" :: color :: e1 :: e2 :: e3 :: rest ->
            placementFromTokens id [ e1, e2, e3 ] (color :: rest)

        _ ->
            Nothing


placementFromTokens : Int -> List String -> List String -> Maybe Placement
placementFromTokens id extra tokens =
    case Parser.parseLine (String.join " " ("1" :: tokens)) of
        Just (SubFileRef data) ->
            Just
                { id = id
                , part = data.file
                , fileText = String.join " " (List.drop 13 tokens)
                , color = data.color
                , matrix = data.transform
                , extra = extra
                }

        _ ->
            Nothing


placementFromData : Int -> SubFileData -> Placement
placementFromData id data =
    { id = id
    , part = data.file
    , fileText = data.file
    , color = data.color
    , matrix = data.transform
    , extra = []
    }


normalise : String -> String
normalise name =
    case Parser.parseLine ("1 0 0 0 0 1 0 0 0 1 0 0 0 1 " ++ name) of
        Just (SubFileRef data) ->
            data.file

        _ ->
            String.toLower name
