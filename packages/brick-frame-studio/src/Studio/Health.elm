module Studio.Health exposing
    ( Fix
    , FixReason(..)
    , Issue(..)
    , check
    , describe
    , plan
    )

{-| Build health for Brick Play's "Tidy up": which parts of a document sit off
the stud grid or inside another part, and the moves that put them right.

Only the main submodel is checked. Glued parts are included: they are obstacles
and supports, but are never moved.

`check` reports:

  - `Misaligned` — a studded part (one with stud points and no Technic points)
    that connects to nothing and is not where studs put it. Either it rests on
    the ground with its origin off the stud lattice (`Connectivity.studOffset`),
    or it sits on studs below (a stud of another part at its anti-stud height,
    under its footprint). A part connects when one of its anti-studs lies within
    `coincideTolerance` (0.5 LDU) of a stud of another part, so intentional
    offsets such as a jumper plate's are healthy. A part with nothing under it is
    not reported: Tidy up never drops a floating part.
  - `Overlapping` — two parts whose effective boxes (the geometry bounds without
    stud protrusions) intersect by more than `overlapTolerance` (1 LDU). Touching
    faces do not count. A Technic axle or pin is not an overlap with the part it
    connects to (`technicJoined`), so an axle through a gear is fine.

Boxes come from the catalog for catalog parts, otherwise from the part's loaded
geometry. A part whose geometry is not loaded yet is left out of the check until
it arrives.

`plan` returns one `Fix` per part to move, in id order, from its document matrix
to its planned one:

1.  Each misaligned part is snapped (`snapTarget`). On the ground it moves to
    the nearest origin on the stud lattice. On studs below it moves to the
    nearest translation that puts the most anti-studs onto studs, so a
    half-stud-off 1 × 1 brick moves half a stud onto the nearest stud. Its
    height and rotation never change.
2.  Each overlap is resolved by moving one part to the nearest free stud-lattice
    spot (`DragPlane.freeSpotPlacement`): the unglued one, or the higher id (the
    later added) when both are unglued. Moves repeat until nothing overlaps, or
    a budget runs out.

Everything is pure and deterministic: the same document, cache and issues give
the same plan.

-}

import Dict exposing (Dict)
import LDraw.Connectivity as Connectivity exposing (ConnectionType(..), OBB, PartConnectivity)
import LDraw.Document as Document exposing (Document)
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set exposing (Set)
import Studio.DragPlane as DragPlane exposing (Box)


{-| A problem `check` found.
-}
type Issue
    = Misaligned { placement : Int }
    | Overlapping { first : Int, second : Int }


{-| Why a part moves: onto the studs (or the ground lattice), or out of another
part it overlaps.
-}
type FixReason
    = SnapToStud
    | LiftOut { other : Int }


{-| One part to move: its placement id, its document matrix and its planned one.
-}
type alias Fix =
    { placement : Int
    , from : Mat4
    , to : Mat4
    , reason : FixReason
    }


{-| A placement with what the check needs: its connectivity, its effective box
(local and world OBB) and its flags.
-}
type alias Item =
    { id : Int
    , matrix : Mat4
    , conn : PartConnectivity
    , box : Box
    , obb : OBB
    , studs : List Vec3
    , antis : List Vec3
    , glued : Bool
    , studRuled : Bool
    }


{-| A connection point in world space.
-}
type alias WorldPoint =
    { position : Vec3
    , normal : Vec3
    , connType : ConnectionType
    }


{-| How close (LDU) an anti-stud and a stud must be to count as one connection.
-}
coincideTolerance : Float
coincideTolerance =
    0.5


{-| How far (LDU) two boxes must overlap before they count as overlapping.
-}
overlapTolerance : Float
overlapTolerance =
    1.0


{-| How close (LDU) a Technic point must lie to another's axis to be threaded on it.
-}
axisTolerance : Float
axisTolerance =
    1.0


{-| The stud pitch: the lattice a studded part's origin sits on (mod 20).
-}
latticeStep : Float
latticeStep =
    20.0


{-| How far (LDU) past its footprint a support's studs may lie.
-}
footprintMargin : Float
footprintMargin =
    10.0


{-| Deepest sub-file nesting read when measuring a part's geometry.
-}
maxDepth : Int
maxDepth =
    6


{-| The issues in a document: misaligned parts and overlapping pairs.
-}
check : PartCache -> Document -> Set Int -> List Issue
check cache doc glued =
    let
        parts =
            items cache doc glued

        misaligned =
            parts
                |> List.filter (\item -> isMisaligned item (othersOf item parts))
                |> List.map (\item -> Misaligned { placement = item.id })

        overlapping =
            overlapPairs parts
                |> List.map (\( a, b ) -> Overlapping { first = a.id, second = b.id })
    in
    misaligned ++ overlapping


{-| The moves that tidy the document for the given issues (see the module doc).
A misaligned part is snapped; overlaps are resolved from the resulting state.
-}
plan : PartCache -> Document -> Set Int -> List Issue -> List Fix
plan cache doc glued issues =
    let
        original =
            items cache doc glued

        snapIds =
            List.filterMap misalignedId issues
                |> Set.fromList

        ( snapped, snapReasons ) =
            snapStage snapIds original

        budget =
            8 * List.length original + 8

        ( final, reasons ) =
            overlapStage budget snapped snapReasons

        finalById =
            Dict.fromList (List.map (\item -> ( item.id, item )) final)
    in
    original
        |> List.filterMap
            (\item ->
                Maybe.map2
                    (\reason moved ->
                        { placement = item.id
                        , from = item.matrix
                        , to = moved.matrix
                        , reason = reason
                        }
                    )
                    (Dict.get item.id reasons)
                    (Dict.get item.id finalById)
            )


{-| A sentence for a fix, such as "Move 1 x 1 brick half a stud" or "Lift
2 x 4 brick out of another brick". `titleOf` gives a part's readable name from
its file name, and `doc` is the document the fix was planned on.
-}
describe : (String -> String) -> Document -> Fix -> String
describe titleOf doc fix =
    let
        name =
            case Document.getPlacement fix.placement doc of
                Just placement ->
                    titleOf placement.part

                Nothing ->
                    "part"
    in
    case fix.reason of
        LiftOut _ ->
            "Lift " ++ name ++ " out of another brick"

        SnapToStud ->
            "Move " ++ name ++ " " ++ snapAmount (originShift fix.from fix.to)


misalignedId : Issue -> Maybe Int
misalignedId issue =
    case issue of
        Misaligned { placement } ->
            Just placement

        Overlapping _ ->
            Nothing


{-| How the wording names a snap: a stud (10 LDU) on either axis is "half a
stud", since a part's origin is half a stud off the lattice on one axis.
-}
snapAmount : Float -> String
snapAmount shift =
    if abs (shift - 10) <= 1 then
        "half a stud"

    else if shift < 9 then
        "a few LDU"

    else
        "onto the studs"


{-| The larger of the X and Z shifts of a part's origin, in LDU.
-}
originShift : Mat4 -> Mat4 -> Float
originShift from to =
    let
        ( ax, az ) =
            originXZ from

        ( bx, bz ) =
            originXZ to
    in
    max (abs (ax - bx)) (abs (az - bz))



-- ── Items ───────────────────────────────────────────────────────────────────


{-| The placements of the main submodel that have a known box, in id order.
A part whose geometry is not loaded yet is left out.
-}
items : PartCache -> Document -> Set Int -> List Item
items cache doc glued =
    let
        placements =
            Document.mainPlacements doc
                |> List.sortBy .id

        rawBoxes =
            placements
                |> List.map .part
                |> Set.fromList
                |> Set.toList
                |> List.map (\file -> ( file, rawBoxOf cache file ))
                |> Dict.fromList
    in
    placements
        |> List.filterMap
            (\placement ->
                case Dict.get placement.part rawBoxes |> Maybe.andThen identity of
                    Just raw ->
                        Just (itemOf cache glued placement.id placement.part placement.matrix raw)

                    Nothing ->
                        Nothing
            )


itemOf : PartCache -> Set Int -> Int -> String -> Mat4 -> Box -> Item
itemOf cache glued id part matrix raw =
    let
        conn =
            Connectivity.extractPartConnectivity part (linesOf cache)

        box =
            Connectivity.effectiveEnvelope raw conn.connections
    in
    placeItem matrix
        { id = id
        , matrix = matrix
        , conn = conn
        , box = box
        , obb = Connectivity.toOBB box matrix
        , studs = []
        , antis = []
        , glued = Set.member id glued
        , studRuled = Connectivity.isStudded conn && not (hasTechnicPoints conn)
        }


{-| The item at a new matrix, with its box and its world stud points recomputed.
-}
placeItem : Mat4 -> Item -> Item
placeItem matrix item =
    let
        placed =
            { item | matrix = matrix, obb = Connectivity.toOBB item.box matrix }
    in
    { placed
        | studs = worldPoints isStud placed
        , antis = worldPoints isAntiStud placed
    }


withMatrix : Mat4 -> Item -> Item
withMatrix =
    placeItem


{-| A part's raw box: the catalog's for a catalog part, else its loaded geometry.
-}
rawBoxOf : PartCache -> String -> Maybe Box
rawBoxOf cache file =
    case Connectivity.knownCatalogPart file of
        Just catalog ->
            List.head catalog.boundingBoxes

        Nothing ->
            geometryBox cache file


{-| The bounds of a part's loaded geometry, with its sub-files (studs included).
-}
geometryBox : PartCache -> String -> Maybe Box
geometryBox cache file =
    linesOf cache file
        |> Maybe.andThen (\lines -> DragPlane.unionBox (geometryPoints cache 0 Mat4.identity lines []))


geometryPoints : PartCache -> Int -> Mat4 -> List LDrawLine -> List Vec3 -> List Vec3
geometryPoints cache depth matrix lines acc =
    if depth > maxDepth then
        acc

    else
        List.foldl
            (\line points ->
                case line of
                    Triangle t ->
                        List.map (Mat4.transform matrix) [ t.p1, t.p2, t.p3 ] ++ points

                    Quad q ->
                        List.map (Mat4.transform matrix) [ q.p1, q.p2, q.p3, q.p4 ] ++ points

                    SubFileRef sub ->
                        case linesOf cache sub.file of
                            Just subLines ->
                                geometryPoints cache (depth + 1) (Mat4.mul matrix sub.transform) subLines points

                            Nothing ->
                                points

                    _ ->
                        points
            )
            acc
            lines


linesOf : PartCache -> String -> Maybe (List LDrawLine)
linesOf cache name =
    case Dict.get (normaliseName name) cache of
        Just (Loaded lines) ->
            Just lines

        _ ->
            Nothing


normaliseName : String -> String
normaliseName name =
    String.toLower (String.replace "\\" "/" name)


hasTechnicPoints : PartConnectivity -> Bool
hasTechnicPoints conn =
    List.any (\point -> isTechnicType point.connType) conn.connections


isTechnicType : ConnectionType -> Bool
isTechnicType connType =
    case connType of
        TechnicAxleMale _ ->
            True

        TechnicAxleFemale _ ->
            True

        TechnicPinMale _ ->
            True

        TechnicPinFemale _ ->
            True

        _ ->
            False


isStud : ConnectionType -> Bool
isStud connType =
    case connType of
        Stud ->
            True

        _ ->
            False


isAntiStud : ConnectionType -> Bool
isAntiStud connType =
    case connType of
        AntiStud ->
            True

        _ ->
            False



-- ── Stud rule ───────────────────────────────────────────────────────────────


worldPoints : (ConnectionType -> Bool) -> Item -> List Vec3
worldPoints keep item =
    item.conn.connections
        |> List.filter (\point -> keep point.connType)
        |> List.map (\point -> (Connectivity.transformPoint item.matrix item.id point).worldPosition)


studsOf : Item -> List Vec3
studsOf item =
    item.studs


antiStudsOf : Item -> List Vec3
antiStudsOf item =
    item.antis


{-| The bottom of the box (the largest Y, as LDraw's Y points down) is on the ground.
-}
restsOnGround : Item -> Bool
restsOnGround item =
    abs (Vec3.getY (DragPlane.worldBox item.matrix item.box).max) <= coincideTolerance


{-| Whether the origin's X and Z sit on the stud lattice for the part's rotation.
-}
latticeOk : Item -> Bool
latticeOk item =
    let
        ( x, z ) =
            originXZ item.matrix

        offset =
            Connectivity.studOffset item.conn item.matrix
    in
    abs (DragPlane.snapTo offset.x latticeStep x - x)
        <= coincideTolerance
        && abs (DragPlane.snapTo offset.z latticeStep z - z)
        <= coincideTolerance


{-| Whether one of the part's anti-studs sits on a stud of another part.
-}
connects : Item -> List Item -> Bool
connects item others =
    let
        studs =
            List.concatMap studsOf others
    in
    List.any
        (\anti -> List.any (\stud -> distance anti stud <= coincideTolerance) studs)
        (antiStudsOf item)


{-| The height the part's anti-studs sit at: the lowest of them (largest Y).
-}
levelOf : Item -> Maybe Float
levelOf item =
    antiStudsOf item
        |> List.map Vec3.getY
        |> List.maximum


{-| The studs of other parts at the part's anti-stud height, under its footprint.
-}
supportStuds : Item -> List Item -> List Vec3
supportStuds item others =
    case levelOf item of
        Nothing ->
            []

        Just level ->
            let
                footprint =
                    DragPlane.worldBox item.matrix item.box
            in
            List.concatMap studsOf others
                |> List.filter
                    (\stud ->
                        abs (Vec3.getY stud - level)
                            <= coincideTolerance
                            && inFootprint footprint stud
                    )


inFootprint : Box -> Vec3 -> Bool
inFootprint box point =
    Vec3.getX point
        >= Vec3.getX box.min
        - footprintMargin
        && Vec3.getX point
        <= Vec3.getX box.max
        + footprintMargin
        && Vec3.getZ point
        >= Vec3.getZ box.min
        - footprintMargin
        && Vec3.getZ point
        <= Vec3.getZ box.max
        + footprintMargin


isMisaligned : Item -> List Item -> Bool
isMisaligned item others =
    item.studRuled
        && not (connects item others)
        && (if restsOnGround item then
                not (latticeOk item)

            else
                not (List.isEmpty (supportStuds item others))
           )


{-| The planned matrix that snaps a misaligned part, or `Nothing` when no
better place is known.
-}
snapTarget : Item -> List Item -> Maybe Mat4
snapTarget item others =
    if restsOnGround item then
        groundSnap item

    else
        supportSnap item others


groundSnap : Item -> Maybe Mat4
groundSnap item =
    let
        ( x, z ) =
            originXZ item.matrix

        offset =
            Connectivity.studOffset item.conn item.matrix

        targetX =
            DragPlane.snapTo offset.x latticeStep x

        targetZ =
            DragPlane.snapTo offset.z latticeStep z
    in
    if abs (targetX - x) <= coincideTolerance && abs (targetZ - z) <= coincideTolerance then
        Nothing

    else
        Just (translated (targetX - x) (targetZ - z) item.matrix)


{-| Moves a part on the studs below. Each candidate translation puts one
anti-stud on one stud; the best one puts the most anti-studs on studs, then is
the nearest to no move, then is the first in order (so ties are deterministic).
-}
supportSnap : Item -> List Item -> Maybe Mat4
supportSnap item others =
    let
        studs =
            supportStuds item others

        antis =
            antiStudsOf item

        candidates =
            List.concatMap
                (\stud ->
                    List.map
                        (\anti -> ( Vec3.getX stud - Vec3.getX anti, Vec3.getZ stud - Vec3.getZ anti ))
                        antis
                )
                studs

        score ( tx, tz ) =
            antis
                |> List.filter
                    (\anti ->
                        List.any
                            (\stud ->
                                abs (Vec3.getX stud - (Vec3.getX anti + tx))
                                    <= coincideTolerance
                                    && abs (Vec3.getZ stud - (Vec3.getZ anti + tz))
                                    <= coincideTolerance
                            )
                            studs
                    )
                |> List.length

        ranked =
            candidates
                |> List.map (\t -> ( score t, t ))
                |> List.sortWith rank
    in
    case ranked of
        ( best, ( tx, tz ) ) :: _ ->
            if best > 0 && (abs tx > coincideTolerance || abs tz > coincideTolerance) then
                Just (translated tx tz item.matrix)

            else
                Nothing

        [] ->
            Nothing


rank : ( Int, ( Float, Float ) ) -> ( Int, ( Float, Float ) ) -> Order
rank ( scoreA, ( ax, az ) ) ( scoreB, ( bx, bz ) ) =
    case compare scoreB scoreA of
        EQ ->
            case compare (ax * ax + az * az) (bx * bx + bz * bz) of
                EQ ->
                    compare ( ax, az ) ( bx, bz )

                other ->
                    other

        other ->
            other


snapStage : Set Int -> List Item -> ( List Item, Dict Int FixReason )
snapStage ids list =
    let
        ( acc, reasons ) =
            List.foldl
                (\item ( done, found ) ->
                    if Set.member item.id ids && not item.glued then
                        case snapTarget item (othersOf item list) of
                            Just matrix ->
                                ( withMatrix matrix item :: done, Dict.insert item.id SnapToStud found )

                            Nothing ->
                                ( item :: done, found )

                    else
                        ( item :: done, found )
                )
                ( [], Dict.empty )
                list
    in
    ( List.reverse acc, reasons )



-- ── Overlaps ────────────────────────────────────────────────────────────────


overlapPairs : List Item -> List ( Item, Item )
overlapPairs list =
    List.concatMap
        (\a ->
            list
                |> List.filter (\b -> a.id < b.id && overlaps a b)
                |> List.map (\b -> ( a, b ))
        )
        list


overlaps : Item -> Item -> Bool
overlaps a b =
    Connectivity.testOBB overlapTolerance a.obb b.obb && not (technicJoined a b)


{-| Whether a Technic point of one part is threaded on, or coincides with, a
compatible Technic point of the other: an axle through a gear's hole, a pin in a
beam's hole. A hole threaded by an axle's line counts whatever its own direction,
since a beam's holes need not run along the axle.
-}
technicJoined : Item -> Item -> Bool
technicJoined a b =
    let
        pointsB =
            technicPoints b
    in
    List.any
        (\p -> List.any (\q -> joins p q || joins q p) pointsB)
        (technicPoints a)


technicPoints : Item -> List WorldPoint
technicPoints item =
    item.conn.connections
        |> List.filter (\point -> isTechnicType point.connType)
        |> List.map
            (\point ->
                let
                    world =
                        Connectivity.transformPoint item.matrix item.id point
                in
                { position = world.worldPosition
                , normal = world.worldNormal
                , connType = point.connType
                }
            )


joins : WorldPoint -> WorldPoint -> Bool
joins male female =
    Connectivity.isCompatible male.connType female.connType
        && (distance male.position female.position
                <= coincideTolerance
                || distanceToAxis female.position male.position male.normal
                <= axisTolerance
           )


distanceToAxis : Vec3 -> Vec3 -> Vec3 -> Float
distanceToAxis point origin direction =
    let
        offset =
            Vec3.sub point origin

        along =
            Vec3.dot offset direction
    in
    Vec3.length (Vec3.sub offset (Vec3.scale along direction))


{-| The mover of an overlapping pair, and the part it is moved away from.
-}
moverFor : ( Item, Item ) -> Maybe ( Item, Item )
moverFor ( a, b ) =
    case ( a.glued, b.glued ) of
        ( True, True ) ->
            Nothing

        ( False, True ) ->
            Just ( a, b )

        _ ->
            Just ( b, a )


overlapStage : Int -> List Item -> Dict Int FixReason -> ( List Item, Dict Int FixReason )
overlapStage budget list reasons =
    if budget <= 0 then
        ( list, reasons )

    else
        case List.filterMap moverFor (overlapPairs list) of
            ( mover, other ) :: _ ->
                let
                    spot =
                        DragPlane.freeSpotPlacement
                            { box = mover.box
                            , matrix = mover.matrix
                            , offset = Connectivity.studOffset mover.conn mover.matrix
                            , start = originXZ mover.matrix
                            , gridStep = latticeStep
                            , others = List.map (\item -> DragPlane.worldBox item.matrix item.box) (othersOf mover list)
                            }

                    moved =
                        withMatrix spot mover

                    next =
                        List.map
                            (\item ->
                                if item.id == mover.id then
                                    moved

                                else
                                    item
                            )
                            list
                in
                overlapStage (budget - 1) next (Dict.insert mover.id (LiftOut { other = other.id }) reasons)

            [] ->
                ( list, reasons )



-- ── Geometry helpers ────────────────────────────────────────────────────────


othersOf : Item -> List Item -> List Item
othersOf item list =
    List.filter (\other -> other.id /= item.id) list


translated : Float -> Float -> Mat4 -> Mat4
translated dx dz matrix =
    Mat4.mul (Mat4.makeTranslate3 dx 0 dz) matrix


originXZ : Mat4 -> ( Float, Float )
originXZ matrix =
    let
        origin =
            Mat4.transform matrix (vec3 0 0 0)
    in
    ( Vec3.getX origin, Vec3.getZ origin )


distance : Vec3 -> Vec3 -> Float
distance a b =
    Vec3.length (Vec3.sub a b)
