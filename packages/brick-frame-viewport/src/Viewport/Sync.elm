module Viewport.Sync exposing
    ( Plan
    , Sourced
    , State
    , buildAll
    , changedSubmodels
    , initialState
    , plan
    , sourceKey
    , updateSubmodelCache
    )

{-| Keeping an instanced scene in step with an editable `LDraw.Document`.

The scene is made of placements tagged with the id of the document placement
they came from. After an edit, `plan` compares the document with what the scene
was last built from and says which sources to remove, which to rebuild and which
library parts still have to be fetched first. The viewport applies the plan with
`Render.Instanced.replaceSource`, so an edit costs a few part meshes and a few
proxy cells instead of a rebuild.

Only the main submodel is tracked placement by placement. A reference to another
submodel expands to all of that submodel's parts under one source id; changing
the contents of such a submodel changes the `subfilesSignature`, and the viewport
reloads the whole document instead.

-}

import Dict exposing (Dict)
import LDraw.Document as Document exposing (Document)
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Render.Instanced as Instanced
import Set exposing (Set)


{-| What the scene currently reflects.

  - `built` maps a source id to the fingerprint it was built from.
  - `pending` holds sources that changed but could not be built yet because
    a part was still being fetched.
  - `readyParts` caches parts whose whole sub-file tree is loaded, so the check
    runs once per distinct part rather than once per placement.

-}
type alias State =
    { document : Document
    , built : Dict Int String
    , pending : Set Int
    , subfilesSignature : String
    , readyParts : Set String
    }


{-| A document placement and the instances it expands to.
-}
type alias Sourced =
    { source : Int
    , fingerprint : String
    , raws : List Instanced.RawPlacement
    }


{-| What to do to the scene, and what to fetch first.
-}
type alias Plan =
    { removed : List Int
    , replaced : List Sourced
    , missing : List String
    , pending : Set Int
    , readyParts : Set String
    }


{-| The state after a full build of `document`: placements whose parts are all
ready are marked built; any with unresolved parts remain in `pending` so the
next `plan` / `reconcile` picks them up and fetches what is missing.
-}
initialState : Document -> Set String -> List Sourced -> State
initialState document ready sourced =
    let
        isResolved s =
            not (List.isEmpty s.raws)
                && List.all (\raw -> Set.member raw.part ready) s.raws

        ( builtSourced, unbuiltSourced ) =
            List.partition isResolved sourced
    in
    { document = document
    , built = Dict.fromList (List.map (\s -> ( s.source, s.fingerprint )) builtSourced)
    , pending = Set.fromList (List.map .source unbuiltSourced)
    , subfilesSignature = Document.subfilesSignature document
    , readyParts = ready
    }


{-| Expand every main placement, for the first build. Parts that have not loaded
simply produce placements that `Instanced` skips.
-}
buildAll : PartCache -> Set String -> Document -> List Sourced
buildAll cache submodelNames document =
    Document.mainPlacements document
        |> List.map (expand cache submodelNames)


{-| Work out the edit that brings the scene up to date with `document`.

Only the placements the document says it touched (plus any still waiting for a
part, and any placements referencing changed submodels) are examined, so the
cost of an edit does not grow with the size of the model beyond a single pass over
the placement list.

When a submodel changes, all document placements referencing it are included in
`replaced` and re-expanded from the updated cache, so editing a submodel propagates
across all instances without requiring a full document reload.

A source whose parts are not all loaded is left out of `replaced` and kept in
`pending`; the next `plan` retries it once the fetches in `missing` finish.

-}
plan : PartCache -> Set String -> Document -> State -> Plan
plan cache submodelNames document state =
    let
        changedSubs =
            changedSubmodels state.document document

        referencesChangedSub name =
            referencesAnySubmodel cache changedSubs name

        submodelCandidates =
            if Set.isEmpty changedSubs then
                Set.empty

            else
                Document.mainPlacements document
                    |> List.filter (\p -> referencesChangedSub p.part)
                    |> List.map .id
                    |> Set.fromList

        candidates =
            Set.union (Set.union document.touched state.pending) submodelCandidates

        present =
            Document.mainPlacements document
                |> List.filter (\p -> Set.member p.id candidates)

        presentIds =
            Set.fromList (List.map .id present)

        removed =
            Set.toList candidates
                |> List.filter (\id -> not (Set.member id presentIds) && Dict.member id state.built)

        changed =
            present
                |> List.filter
                    (\p ->
                        (Dict.get p.id state.built /= Just (Document.fingerprint p))
                            || Set.member p.id submodelCandidates
                    )

        step p acc =
            let
                sourced =
                    expand cache submodelNames p

                parts =
                    sourced.raws |> List.map .part |> dedupe

                status =
                    List.foldl (partStatus cache) { missing = [], waiting = False, ready = acc.ready } parts
            in
            if List.isEmpty status.missing && not status.waiting then
                { acc | replaced = sourced :: acc.replaced, ready = status.ready }

            else
                { acc
                    | missing = status.missing ++ acc.missing
                    , pending = Set.insert p.id acc.pending
                    , ready = status.ready
                }

        result =
            List.foldl step { replaced = [], missing = [], ready = state.readyParts, pending = Set.empty } changed
    in
    { removed = removed
    , replaced = List.reverse result.replaced
    , missing = dedupe result.missing
    , pending = result.pending
    , readyParts = result.ready
    }


{-| Submodels whose lines or existence differ between two documents.
-}
changedSubmodels : Document -> Document -> Set String
changedSubmodels oldDoc newDoc =
    let
        oldSubmodels =
            Dict.fromList (List.map (\s -> ( Document.normalise s.name, Document.toLinesOf s.name oldDoc )) oldDoc.submodels)

        newSubmodels =
            Dict.fromList (List.map (\s -> ( Document.normalise s.name, Document.toLinesOf s.name newDoc )) newDoc.submodels)

        isChanged ( name, newLines ) =
            case Dict.get name oldSubmodels of
                Just oldLines ->
                    oldLines /= newLines

                Nothing ->
                    True

        modifiedOrAdded =
            newSubmodels
                |> Dict.toList
                |> List.filter isChanged
                |> List.map Tuple.first

        deleted =
            oldSubmodels
                |> Dict.keys
                |> List.filter (\k -> not (Dict.member k newSubmodels))
    in
    Set.fromList (modifiedOrAdded ++ deleted)


{-| Seed or update non-main submodels in PartCache with parsed lines from Document.
-}
updateSubmodelCache : Document -> PartCache -> PartCache
updateSubmodelCache document cache =
    let
        subfiles =
            Document.subfiles document
    in
    List.foldl (\( name, lines ) acc -> Dict.insert name (Loaded lines) acc) cache subfiles


{-| Check whether a part or submodel reference transitively references any of the target submodels.
-}
referencesAnySubmodel : PartCache -> Set String -> String -> Bool
referencesAnySubmodel cache targets partName =
    let
        norm =
            Document.normalise partName
    in
    if Set.member norm targets then
        True

    else
        case Dict.get norm cache of
            Just (Loaded lines) ->
                lines
                    |> List.any
                        (\line ->
                            case line of
                                SubFileRef ref ->
                                    referencesAnySubmodel cache targets ref.file

                                _ ->
                                    False
                        )

            _ ->
                False


{-| The id a source is stored under, for a document placement.
-}
sourceKey : Document.Placement -> Int
sourceKey =
    .id


expand : PartCache -> Set String -> Document.Placement -> Sourced
expand cache submodelNames p =
    { source = p.id
    , fingerprint = Document.fingerprint p
    , raws =
        Instanced.extractPlacements submodelNames
            [ SubFileRef { color = p.color, transform = p.matrix, file = p.part } ]
            cache
            15
    }


{-| Whether the whole tree under a part is loaded, and what is absent from the
cache. Parts that failed to load count as settled — the viewer skips what it
cannot find, and waiting on them would block the edit forever.
-}
partStatus :
    PartCache
    -> String
    -> { missing : List String, waiting : Bool, ready : Set String }
    -> { missing : List String, waiting : Bool, ready : Set String }
partStatus cache name acc =
    if Set.member name acc.ready then
        acc

    else
        let
            ( missing, waiting ) =
                walk cache [ name ] Set.empty [] False
                    |> (\( m, w ) -> ( m, w ))
        in
        { missing = missing ++ acc.missing
        , waiting = acc.waiting || waiting
        , ready =
            if List.isEmpty missing && not waiting then
                Set.insert name acc.ready

            else
                acc.ready
        }


walk : PartCache -> List String -> Set String -> List String -> Bool -> ( List String, Bool )
walk cache todo seen missing waiting =
    case todo of
        [] ->
            ( missing, waiting )

        name :: rest ->
            if Set.member name seen then
                walk cache rest seen missing waiting

            else
                case Dict.get name cache of
                    Nothing ->
                        walk cache rest (Set.insert name seen) (name :: missing) waiting

                    Just Loading ->
                        walk cache rest (Set.insert name seen) missing True

                    Just (Failed _) ->
                        walk cache rest (Set.insert name seen) missing waiting

                    Just (Loaded lines) ->
                        walk cache (subFileNames lines ++ rest) (Set.insert name seen) missing waiting


subFileNames : List LDrawLine -> List String
subFileNames lines =
    List.filterMap
        (\line ->
            case line of
                SubFileRef ref ->
                    Just ref.file

                _ ->
                    Nothing
        )
        lines


dedupe : List String -> List String
dedupe names =
    Set.toList (Set.fromList names)
