module Viewport.Sync exposing
    ( Plan
    , Sourced
    , State
    , activeSourcesFor
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

When `activePath` steps into a submodel instance, placements along that chain
are opened up so the active submodel's own placements become individually
addressable sources in world space, while everything outside the chain stays
under its enclosing placement id and renders ghosted.

-}

import Dict exposing (Dict)
import LDraw.Document as Document exposing (Document)
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Render.Instanced as Instanced
import Set exposing (Set)


{-| What the scene currently reflects.

  - `activePath` is the chain of submodel instance placement ids leading to the
    active submodel (`[]` at the top level).
  - `built` maps a source id to the fingerprint it was built from.
  - `pending` holds sources that changed but could not be built yet because
    a part was still being fetched.
  - `readyParts` caches parts whose whole sub-file tree is loaded, so the check
    runs once per distinct part rather than once per placement.

-}
type alias State =
    { document : Document
    , activePath : List Int
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


type alias ContextualPlacement =
    { placement : Document.Placement
    , parentTransform : Mat4
    , parentColor : Int
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
    , activePath = []
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
    effectivePlacements [] document
        |> Tuple.first
        |> List.map (expandContextual cache submodelNames)


{-| The set of active placement source ids for a submodel navigation path, or
`Nothing` when at the top level.
-}
activeSourcesFor : List Int -> Document -> Maybe (Set Int)
activeSourcesFor activePath document =
    effectivePlacements activePath document
        |> Tuple.second


{-| Work out the edit that brings the scene up to date with `document`.

Only the placements that changed (plus any still waiting for a part, and any
placements referencing changed submodels) are rebuilt, so the cost of an edit
does not grow with the size of the model beyond a single pass over the placement
list.

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

        ( effective, _ ) =
            effectivePlacements state.activePath document

        presentIds =
            Set.fromList (List.map (.placement >> .id) effective)

        removed =
            Dict.keys state.built
                |> List.filter (\id -> not (Set.member id presentIds))

        needsRebuild cp =
            let
                id =
                    cp.placement.id
            in
            (Dict.get id state.built /= Just (contextualFingerprint cp))
                || Set.member id state.pending
                || Set.member id document.touched
                || (not (Set.isEmpty changedSubs) && referencesChangedSub cp.placement.part)

        changed =
            List.filter needsRebuild effective

        step cp acc =
            let
                sourced =
                    expandContextual cache submodelNames cp

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
                    , pending = Set.insert cp.placement.id acc.pending
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


effectivePlacements : List Int -> Document -> ( List ContextualPlacement, Maybe (Set Int) )
effectivePlacements activePath document =
    let
        resolveColor baseColor color =
            if color == 16 || color == -1 then
                baseColor

            else
                color

        walkActivePath path submodelName parentTransform parentColor depth visited =
            let
                currentPlacements =
                    Document.placementsOf submodelName document

                wrap p =
                    { placement = p
                    , parentTransform = parentTransform
                    , parentColor = parentColor
                    }
            in
            case path of
                [] ->
                    ( List.map wrap currentPlacements
                    , if depth > 0 then
                        Just (Set.fromList (List.map .id currentPlacements))

                      else
                        Nothing
                    )

                stepId :: restIds ->
                    case List.filter (\p -> p.id == stepId) currentPlacements of
                        targetP :: _ ->
                            case Document.submodelByName targetP.part document of
                                Just childSub ->
                                    let
                                        childNorm =
                                            Document.normalise childSub.name
                                    in
                                    if Set.member childNorm visited then
                                        ( List.map wrap currentPlacements, Nothing )

                                    else
                                        let
                                            siblings =
                                                currentPlacements
                                                    |> List.filter (\p -> p.id /= stepId)
                                                    |> List.map wrap

                                            nextTransform =
                                                Mat4.mul parentTransform targetP.matrix

                                            nextColor =
                                                resolveColor parentColor targetP.color

                                            ( inner, activeSet ) =
                                                walkActivePath restIds childSub.name nextTransform nextColor (depth + 1) (Set.insert childNorm visited)
                                        in
                                        ( siblings ++ inner, activeSet )

                                Nothing ->
                                    ( List.map wrap currentPlacements, Nothing )

                        [] ->
                            ( List.map wrap currentPlacements, Nothing )

        mainName =
            (Document.mainSubmodel document).name
    in
    walkActivePath activePath mainName Mat4.identity 15 0 (Set.singleton (Document.normalise mainName))


contextualFingerprint : ContextualPlacement -> String
contextualFingerprint { placement, parentTransform, parentColor } =
    let
        worldMatrix =
            Mat4.mul parentTransform placement.matrix

        resolvedColor =
            if placement.color == 16 || placement.color == -1 then
                parentColor

            else
                placement.color
    in
    Document.fingerprint { placement | matrix = worldMatrix, color = resolvedColor }


expandContextual : PartCache -> Set String -> ContextualPlacement -> Sourced
expandContextual cache submodelNames cp =
    let
        worldMatrix =
            Mat4.mul cp.parentTransform cp.placement.matrix

        resolvedColor =
            if cp.placement.color == 16 || cp.placement.color == -1 then
                cp.parentColor

            else
                cp.placement.color
    in
    { source = cp.placement.id
    , fingerprint = contextualFingerprint cp
    , raws =
        Instanced.extractPlacements submodelNames
            [ SubFileRef { color = resolvedColor, transform = worldMatrix, file = cp.placement.part } ]
            cache
            cp.parentColor
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
