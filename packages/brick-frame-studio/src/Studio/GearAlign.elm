module Studio.GearAlign exposing (alignmentCommands)

{-| Tooth alignment of gears that a committed edit has just meshed.

After an edit that moves parts (a drop, a nudge, a turn, an added part), the
meshes of the new document are compared with those of the document before it.
Each new mesh that involves a changed part is closed by turning the moved
gear's rigid group about its axle: every gear and axle on the same axis line
(see `Gear.Phase` for the tooth geometry). A whole group turns because an
axle re-seats in 90° steps only, which never changes a tooth phase for the
standard gears.

The result is pure. `alignmentCommands` returns the `RotateAroundAxis`
commands, in order, to record in the same history entry as the edit. Worms
are never turned, and a rotation under half a degree is dropped. An edit that
moves no gear or axle returns no commands without looking for meshes.

-}

import Data
import Dict exposing (Dict)
import Gear.Detect as Detect
import Gear.Phase as Phase exposing (Constraint)
import Gear.Placements as Placements
import Gear.Types exposing (GearInstance)
import LDraw.Connectivity as Connectivity
import LDraw.Document as Document exposing (Document, Placement)
import LDraw.Resolve exposing (PartCache)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set exposing (Set)
import Studio.Edit as Edit exposing (Command(..))


{-| Rotations smaller than this are not worth recording.
-}
minimumRadians : Float
minimumRadians =
    degrees 0.5


{-| Above this many meshes on one rigid group only the new mesh is closed.
The group search is exponential in the number of meshes.
-}
maxGroupMeshes : Int
maxGroupMeshes =
    6


{-| Slack, in LDU, when deciding whether two parts touch along an axle line.
-}
touchTolerance : Float
touchTolerance =
    2.0


{-| An axis line: a point on it and its unit direction.
-}
type alias Line =
    { centre : Vec3
    , axis : Vec3
    }


{-| The gears, axles and meshes of one document. Gears and axles are keyed by
placement id; meshes are pairs of placement ids, smaller first.
-}
type alias Snapshot =
    { gears : Dict Int GearInstance
    , axles : Dict Int Line
    , meshes : Set ( Int, Int )
    , spans : Dict Int (List Vec3)
    }


{-| The `RotateAroundAxis` commands that close the new meshes of an edit.

`before` is the document the edit started from, `after` the document it
produced. Each command is meant for the same `Group` as the edit, applied
in order after it.

-}
alignmentCommands : PartCache -> { before : Document, after : Document } -> List Command
alignmentCommands cache { before, after } =
    let
        changed =
            changedIds before after

        movesGearOrAxle =
            Set.toList changed
                |> List.filterMap (\id -> Document.getPlacement id after)
                |> List.any (\p -> isGearPart p.part || isAxle p.part)
    in
    if not movesGearOrAxle then
        []

    else
        let
            oldMeshes =
                (snapshot cache before).meshes

            newMeshes =
                (snapshot cache after).meshes
                    |> (\meshes -> Set.diff meshes oldMeshes)
                    |> Set.toList
                    |> List.filter (\( a, b ) -> Set.member a changed || Set.member b changed)
        in
        alignNew cache changed newMeshes after Set.empty


{-| Close the new meshes one by one. Each turn is applied to the document
before the next mesh is looked at, and a rigid group turns at most once.
-}
alignNew : PartCache -> Set Int -> List ( Int, Int ) -> Document -> Set Int -> List Command
alignNew cache changed pairs doc turned =
    case pairs of
        [] ->
            []

        ( a, b ) :: rest ->
            let
                moving =
                    if Set.member a changed then
                        a

                    else
                        b

                partner =
                    if moving == a then
                        b

                    else
                        a

                snap =
                    snapshot cache doc
            in
            case lineMembers snap moving of
                Nothing ->
                    alignNew cache changed rest doc turned

                Just ( line, members ) ->
                    let
                        key =
                            List.minimum members |> Maybe.withDefault moving
                    in
                    if Set.member key turned || List.member partner members then
                        alignNew cache changed rest doc turned

                    else
                        case turnGroup snap line members moving partner of
                            Nothing ->
                                alignNew cache changed rest doc (Set.insert key turned)

                            Just command ->
                                let
                                    ( turnedDoc, _ ) =
                                        Edit.apply command doc
                                in
                                command :: alignNew cache changed rest turnedDoc (Set.insert key turned)


{-| The rotation of a rigid group that closes its new mesh, or `Nothing` when
the mesh has a worm, or the group is already aligned to within
`minimumRadians`.

Every mesh between the group and a gear outside it is a constraint. With at
most `maxGroupMeshes` of them the rotation minimises the worst remaining tooth
misalignment over all of them; beyond that only the new mesh is closed.

-}
turnGroup : Snapshot -> Line -> List Int -> Int -> Int -> Maybe Command
turnGroup snap line members moving partner =
    let
        inGroup id =
            List.member id members

        groupMeshes =
            Set.toList snap.meshes
                |> List.filterMap
                    (\( p, q ) ->
                        if inGroup p && not (inGroup q) then
                            Just ( p, q )

                        else if inGroup q && not (inGroup p) then
                            Just ( q, p )

                        else
                            Nothing
                    )

        optionsOf ( movingId, fixedId ) =
            Maybe.map2
                (\movingGear fixedGear ->
                    Phase.meshConstraints { fixed = fixedGear, moving = movingGear }
                        |> Maybe.map (List.map (orientConstraint line movingGear))
                )
                (Dict.get movingId snap.gears)
                (Dict.get fixedId snap.gears)
                |> Maybe.andThen identity

        newOptions =
            optionsOf ( moving, partner )

        chosen =
            if List.length groupMeshes <= maxGroupMeshes then
                List.filterMap optionsOf groupMeshes

            else
                Maybe.map List.singleton newOptions |> Maybe.withDefault []
    in
    case newOptions of
        Just _ ->
            let
                correction =
                    Phase.groupCorrection chosen
            in
            if abs correction.radians < minimumRadians then
                Nothing

            else
                Just
                    (RotateAroundAxis members
                        { pivot = line.centre
                        , axis = line.axis
                        , radians = correction.radians
                        }
                    )

        Nothing ->
            -- Worms (or a mesh without a contact point): no tooth phase to close.
            Nothing


{-| Express a moving gear's residual for a rotation about the group's axis.
The phase residual is measured about the gear's own axis; a gear whose axis
points against the group's axis turns the other way.
-}
orientConstraint : Line -> GearInstance -> Constraint -> Constraint
orientConstraint line movingGear constraint =
    let
        orientation =
            if Vec3.dot (gearLine movingGear).axis line.axis < 0 then
                -1

            else
                1
    in
    { constraint | movingResidual = orientation * constraint.movingResidual }



-- ── Snapshots and axis lines ──────────────────────────────────────────────────


snapshot : PartCache -> Document -> Snapshot
snapshot cache doc =
    let
        instances =
            Detect.extractGears Data.gearParts (Document.toLines doc) cache

        sources =
            Placements.gearSources doc instances

        byGearId =
            Dict.fromList (List.map (\g -> ( g.id, g )) instances)

        gears =
            Dict.toList sources
                |> List.filterMap (\( gearId, placementId ) -> Dict.get gearId byGearId |> Maybe.map (\g -> ( placementId, g )))
                |> Dict.fromList

        graph =
            Detect.buildGearGraph instances

        meshes =
            Dict.toList graph.connections
                |> List.concatMap (\( gearId, others ) -> List.map (\other -> ( gearId, other )) others)
                |> List.filterMap
                    (\( a, b ) ->
                        Maybe.map2 orderPair (Dict.get a sources) (Dict.get b sources)
                    )
                |> Set.fromList

        axles =
            Document.mainPlacements doc
                |> List.filter (\p -> isAxle p.part)
                |> List.map (\p -> ( p.id, axleOf p ))
                |> Dict.fromList

        spans =
            Document.mainPlacements doc
                |> List.filter (\p -> Dict.member p.id gears || Dict.member p.id axles)
                |> List.map (\p -> ( p.id, worldCorners p ))
                |> Dict.fromList
    in
    { gears = gears, axles = axles, meshes = meshes, spans = spans }


isGearPart : String -> Bool
isGearPart part =
    List.any (\gear -> gear.partFile == part) Data.gearParts


{-| The eight corners of an axis-aligned box.
-}
cornerPoints : Connectivity.BoundingBox3D -> List Vec3
cornerPoints box =
    List.concatMap
        (\x ->
            List.concatMap
                (\y -> List.map (\z -> vec3 x y z) [ Vec3.getZ box.min, Vec3.getZ box.max ])
                [ Vec3.getY box.min, Vec3.getY box.max ]
        )
        [ Vec3.getX box.min, Vec3.getX box.max ]


{-| The world-space corners of a placement's catalogued bounding boxes. A part
without a catalogue entry is represented by its origin.
-}
worldCorners : Placement -> List Vec3
worldCorners placement =
    let
        boxes =
            Connectivity.knownCatalogPart placement.part
                |> Maybe.map .boundingBoxes
                |> Maybe.withDefault []

        cornersOf box =
            List.map (Mat4.transform placement.matrix) (cornerPoints box)
    in
    case boxes of
        [] ->
            [ Mat4.transform placement.matrix (vec3 0 0 0) ]

        _ ->
            List.concatMap cornersOf boxes


orderPair : Int -> Int -> ( Int, Int )
orderPair a b =
    if a < b then
        ( a, b )

    else
        ( b, a )


{-| Placements whose matrix changed, or which are new, between two documents.
-}
changedIds : Document -> Document -> Set Int
changedIds before after =
    Document.placements after
        |> List.filter
            (\p ->
                case Document.getPlacement p.id before of
                    Just old ->
                        old.matrix /= p.matrix

                    Nothing ->
                        True
            )
        |> List.map .id
        |> Set.fromList


{-| The axis line of a gear: its axle centre and its rotation axis.
-}
gearLine : GearInstance -> Line
gearLine gear =
    let
        frame =
            Phase.gearFrame gear
    in
    { centre = frame.centre, axis = frame.axis }


axleOf : Placement -> Line
axleOf placement =
    let
        origin =
            Mat4.transform placement.matrix (vec3 0 0 0)

        along =
            Vec3.sub (Mat4.transform placement.matrix (vec3 1 0 0)) origin
    in
    { centre = origin
    , axis =
        if Vec3.length along < 1.0e-9 then
            vec3 1 0 0

        else
            Vec3.normalize along
    }


axleParts : List String
axleParts =
    [ "3704.dat", "3705.dat", "3706.dat", "3707.dat" ]


{-| Axle parts: the cross axles, and any catalogued part with an axle male along
its local X.
-}
isAxle : String -> Bool
isAxle part =
    List.member part axleParts
        || (Connectivity.knownCatalogPart part
                |> Maybe.map (.connections >> List.any axleMaleAlongX)
                |> Maybe.withDefault False
           )


axleMaleAlongX : Connectivity.ConnectionPoint -> Bool
axleMaleAlongX point =
    case point.connType of
        Connectivity.TechnicAxleMale _ ->
            abs (Vec3.dot point.normal (vec3 1 0 0)) > 0.99

        _ ->
            False


{-| The gears and axles that turn with a gear: the parts on its axis line
(parallel to within 0.99 and at most 2 LDU off the line) that are connected to
it along the line. Connected means their extents along the axis overlap, by
chaining outward from the gear. Parts on the same line with a gap between them
do not turn.
-}
lineMembers : Snapshot -> Int -> Maybe ( Line, List Int )
lineMembers snap placementId =
    Dict.get placementId snap.gears
        |> Maybe.map
            (\gear ->
                let
                    line =
                        gearLine gear

                    gearIds =
                        Dict.toList snap.gears
                            |> List.filter (\( _, other ) -> sameLine line (gearLine other))
                            |> List.map Tuple.first

                    axleIds =
                        Dict.toList snap.axles
                            |> List.filter (\( _, axle ) -> sameLine line axle)
                            |> List.map Tuple.first

                    spanOf id =
                        axialSpan line.axis (Dict.get id snap.spans |> Maybe.withDefault [])

                    candidates =
                        List.filter (\id -> id /= placementId) (gearIds ++ axleIds)
                in
                ( line
                , connectedChain (spanOf placementId) (List.map (\id -> ( id, spanOf id )) candidates)
                    |> List.map Tuple.first
                    |> (::) placementId
                )
            )


type alias Span =
    ( Float, Float )


{-| The interval a set of points covers along an axis.
-}
axialSpan : Vec3 -> List Vec3 -> Span
axialSpan axis points =
    case List.map (\p -> Vec3.dot p axis) points of
        [] ->
            ( 0, 0 )

        first :: rest ->
            List.foldl (\v ( lo, hi ) -> ( min lo v, max hi v )) ( first, first ) rest


{-| The candidates that can be reached from the start span by a chain of
overlapping spans, in the order they were reached.
-}
connectedChain : Span -> List ( Int, Span ) -> List ( Int, Span )
connectedChain start candidates =
    growChain [ ( -1, start ) ] candidates


growChain : List ( Int, Span ) -> List ( Int, Span ) -> List ( Int, Span )
growChain reached remaining =
    let
        touches ( _, span ) =
            List.any (\( _, other ) -> overlaps other span) reached

        ( hit, miss ) =
            List.partition touches remaining
    in
    if List.isEmpty hit then
        List.drop 1 reached

    else
        growChain (reached ++ hit) miss


overlaps : Span -> Span -> Bool
overlaps ( aLo, aHi ) ( bLo, bHi ) =
    aLo <= bHi + touchTolerance && bLo <= aHi + touchTolerance


sameLine : Line -> Line -> Bool
sameLine line other =
    abs (Vec3.dot line.axis other.axis)
        >= 0.99
        && Vec3.length (perpendicularTo line.axis (Vec3.sub other.centre line.centre))
        <= 2.0


perpendicularTo : Vec3 -> Vec3 -> Vec3
perpendicularTo axis w =
    Vec3.sub w (Vec3.scale (Vec3.dot w axis) axis)
