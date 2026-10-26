module Simulation.Meshes exposing
    ( ComponentMeshRender
    , ComponentRender
    , DrivenAxle
    , GearRender
    , buildCoaxialMeshRenders
    , buildComponentMeshRenders
    , buildComponentRenders
    , buildDrivenAxles
    , buildGearMeshes
    , rotationAround
    , toYUpPoint
    , topLevelCoaxialGear
    )

{-| Meshes for the moving parts of a gear train.

Gears, axles and the parts that ride on them are drawn separately from the
static scene so they can be rotated each frame about their own axis. This module
builds those meshes once, when a model has loaded, together with the axis and
centre each one turns about. Each moving part carries a triangle mesh and an
edge mesh (its LDraw edge lines as screen-aligned quads), so it is outlined like
the static scene. A gear turns about its own true axis: the Y-up image of its local +Z through
its world matrix, not a canonicalised sign. The gear angle from
`Gear.Physics` is measured about that axis, so a flipped gear turns the right
way round. Parts that ride on a gear are turned about the same line: their
axis is oriented to the driving gear's true axis (`alignToGear`), so the same
angle applies to them.

-}

import Dict exposing (Dict)
import Gear.Components as Components
import Gear.Types exposing (GearId, GearInstance)
import LDraw.Geometry as Geometry
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Render.EdgeShader exposing (EdgeVertex)
import Render.Mesh exposing (Vertex)
import WebGL


{-| Mesh of one gear with the point and axis it spins about.
-}
type alias GearRender =
    { mesh : WebGL.Mesh Vertex
    , lodMesh : WebGL.Mesh Vertex
    , surface : List ( Vertex, Vertex, Vertex )
    , edges : WebGL.Mesh EdgeVertex
    , center : Vec3.Vec3
    , axis : Vec3.Vec3
    , pitchRadius : Float
    }


{-| Arrow guide for one axle or pin.
-}
type alias ComponentRender =
    { mesh : WebGL.Mesh EdgeVertex
    , center : Vec3.Vec3
    , axis : Vec3.Vec3
    , drivingGearId : Maybe GearId
    }


{-| Mesh of a non-gear part that rotates with a gear.
-}
type alias ComponentMeshRender =
    { mesh : WebGL.Mesh Vertex
    , surface : List ( Vertex, Vertex, Vertex )
    , edges : WebGL.Mesh EdgeVertex
    , center : Vec3.Vec3
    , rotationCenter : Vec3.Vec3
    , axis : Vec3.Vec3
    , drivingGearId : Maybe GearId
    }


buildGearMeshes : PartCache -> PartCache -> List GearInstance -> Dict GearId GearRender
buildGearMeshes lodCache cache instances =
    instances
        |> List.foldl
            (\inst acc ->
                let
                    file =
                        inst.spec.partFile
                in
                case Dict.get file cache of
                    Just (Loaded lines) ->
                        let
                            geom =
                                Geometry.flatten lines cache inst.color inst.worldMatrix

                            -- Low-detail mesh from the decimated part text, falling
                            -- back to the full geometry when no LOD variant exists.
                            -- Sub-file refs are still resolved against the full cache.
                            lodTriangles =
                                case Dict.get file lodCache of
                                    Just (Loaded lodLines) ->
                                        (Geometry.flatten lodLines cache inst.color inst.worldMatrix).triangles

                                    _ ->
                                        geom.triangles

                            center =
                                toYUpPoint (Mat4.transform inst.worldMatrix (Vec3.vec3 0 0 0))

                            axisEnd =
                                toYUpPoint (Mat4.transform inst.worldMatrix (Vec3.vec3 0 0 1))

                            axisRaw =
                                Vec3.sub axisEnd center

                            axisLen =
                                Vec3.length axisRaw

                            axis =
                                if axisLen < 1.0e-6 then
                                    Vec3.vec3 0 1 0

                                else
                                    Vec3.scale (1 / axisLen) axisRaw
                        in
                        Dict.insert
                            inst.id
                            { mesh = WebGL.triangles geom.triangles
                            , surface = geom.triangles
                            , lodMesh = WebGL.triangles lodTriangles
                            , edges = edgeMesh geom.lines
                            , center = center
                            , axis = axis
                            , pitchRadius = inst.spec.pitchRadius
                            }
                            acc

                    _ ->
                        acc
            )
            Dict.empty


toYUpPoint : Vec3.Vec3 -> Vec3.Vec3
toYUpPoint p =
    Vec3.vec3 (Vec3.getX p) -(Vec3.getY p) -(Vec3.getZ p)


toYUpAxis : Vec3.Vec3 -> Vec3.Vec3
toYUpAxis axis =
    let
        raw =
            Vec3.vec3 (Vec3.getX axis) -(Vec3.getY axis) -(Vec3.getZ axis)

        len =
            Vec3.length raw
    in
    if len < 1.0e-6 then
        Vec3.vec3 0 1 0

    else
        canonicalizeAxis (Vec3.scale (1 / len) raw)


canonicalizeAxis : Vec3.Vec3 -> Vec3.Vec3
canonicalizeAxis axis =
    let
        ax =
            abs (Vec3.getX axis)

        ay =
            abs (Vec3.getY axis)

        az =
            abs (Vec3.getZ axis)

        signToKeep =
            if ax >= ay && ax >= az then
                Vec3.getX axis

            else if ay >= az then
                Vec3.getY axis

            else
                Vec3.getZ axis
    in
    if signToKeep < 0 then
        Vec3.scale -1 axis

    else
        axis


rotationAround : Float -> Vec3.Vec3 -> Vec3.Vec3 -> Mat4.Mat4
rotationAround angle axis pivot =
    let
        toOrigin =
            Mat4.makeTranslate (Vec3.scale -1 pivot)

        rotate =
            Mat4.makeRotate angle axis

        back =
            Mat4.makeTranslate pivot
    in
    Mat4.mul back (Mat4.mul rotate toOrigin)


buildComponentRenders : PartCache -> List Components.ComponentInstance -> List GearInstance -> List ComponentRender
buildComponentRenders cache components gears =
    components
        |> List.filter (\c -> c.kind == Components.AxleLike)
        |> List.filterMap
            (\component ->
                let
                    axis =
                        alignToGear (componentDrivingGear cache component gears)
                            gears
                            (inferComponentAxis cache component)

                    lines =
                        componentArrowLines component axis
                            |> List.map lineToEdgeVertices
                in
                if List.isEmpty lines then
                    Nothing

                else
                    Just
                        { mesh = WebGL.lines lines
                        , center = toYUpPoint component.worldPosition
                        , axis = axis
                        , drivingGearId = componentDrivingGear cache component gears
                        }
            )


buildComponentMeshRenders : PartCache -> List Components.ComponentInstance -> List GearInstance -> List ComponentMeshRender
buildComponentMeshRenders cache components gears =
    components
        |> List.filter (\c -> c.kind == Components.AxleLike)
        |> List.filterMap
            (\component ->
                case Dict.get component.partFile cache of
                    Just (Loaded lines) ->
                        let
                            geom =
                                Geometry.flatten lines cache component.color component.worldMatrix

                            drivingGearId =
                                componentDrivingGear cache component gears

                            ( rotationCenter, axis ) =
                                case Maybe.andThen (\gid -> gearAxisInfoById gid gears) drivingGearId of
                                    Just info ->
                                        ( info.center, info.axis )

                                    Nothing ->
                                        ( toYUpPoint component.worldPosition, inferComponentAxis cache component )
                        in
                        Just
                            { mesh = WebGL.triangles geom.triangles
                            , surface = geom.triangles
                            , edges = edgeMesh geom.lines
                            , center = toYUpPoint component.worldPosition
                            , rotationCenter = rotationCenter
                            , axis = axis
                            , drivingGearId = drivingGearId
                            }

                    _ ->
                        Nothing
            )


componentArrowLines : Components.ComponentInstance -> Vec3.Vec3 -> List ( Vec3.Vec3, Vec3.Vec3 )
componentArrowLines component axis =
    let
        center =
            toYUpPoint component.worldPosition

        shaftHalfLength =
            10.0

        headLength =
            4.0

        headWidth =
            2.5

        start =
            Vec3.sub center (Vec3.scale shaftHalfLength axis)

        tip =
            Vec3.add center (Vec3.scale shaftHalfLength axis)

        worldUp =
            Vec3.vec3 0 1 0

        perpRawA =
            Vec3.cross axis worldUp

        perpRawB =
            Vec3.cross axis (Vec3.vec3 1 0 0)

        perpRaw =
            if Vec3.length perpRawA < 1.0e-6 then
                perpRawB

            else
                perpRawA

        perpLen =
            Vec3.length perpRaw

        perp =
            if perpLen < 1.0e-6 then
                Vec3.vec3 0 0 1

            else
                Vec3.scale (1 / perpLen) perpRaw

        headBase =
            Vec3.sub tip (Vec3.scale headLength axis)

        headLeft =
            Vec3.add headBase (Vec3.scale headWidth perp)

        headRight =
            Vec3.sub headBase (Vec3.scale headWidth perp)
    in
    [ ( start, tip )
    , ( tip, headLeft )
    , ( tip, headRight )
    ]


{-| Expand flattened edge segments into screen-aligned quads for `Render.EdgeShader`.
-}
edgeMesh : List ( Vec3.Vec3, Vec3.Vec3 ) -> WebGL.Mesh EdgeVertex
edgeMesh lines =
    WebGL.triangles (List.concatMap lineToQuad lines)


{-| Two triangles covering one segment. Mirrors `Render.Instanced`'s quad expansion
(which is not exposed).
-}
lineToQuad : ( Vec3.Vec3, Vec3.Vec3 ) -> List ( EdgeVertex, EdgeVertex, EdgeVertex )
lineToQuad ( p1, p2 ) =
    let
        v : Float -> Vec3.Vec3 -> Vec3.Vec3 -> EdgeVertex
        v side pos other =
            { position = pos, other = other, side = side }
    in
    [ ( v -1 p1 p2, v 1 p1 p2, v 1 p2 p1 )
    , ( v -1 p1 p2, v 1 p2 p1, v -1 p2 p1 )
    ]


lineToEdgeVertices : ( Vec3.Vec3, Vec3.Vec3 ) -> ( EdgeVertex, EdgeVertex )
lineToEdgeVertices ( p1, p2 ) =
    -- GuideShader only reads `position`; `other` and `side` are unused but required by the type.
    ( { position = p1, other = p2, side = -1.0 }
    , { position = p2, other = p1, side = 1.0 }
    )


componentDrivingGear : PartCache -> Components.ComponentInstance -> List GearInstance -> Maybe GearId
componentDrivingGear cache component gears =
    let
        compCenter =
            toYUpPoint component.worldPosition

        compAxis =
            inferComponentAxis cache component

        candidates =
            gears
                |> List.concatMap
                    (\gear ->
                        let
                            gearCenter =
                                toYUpPoint gear.worldPosition

                            gearAxisEnd =
                                toYUpPoint (Mat4.transform gear.worldMatrix (Vec3.vec3 0 0 1))

                            gearAxisRaw =
                                Vec3.sub gearAxisEnd gearCenter

                            gearAxisLen =
                                Vec3.length gearAxisRaw

                            gearAxis =
                                if gearAxisLen < 1.0e-6 then
                                    Vec3.vec3 0 1 0

                                else
                                    Vec3.scale (1 / gearAxisLen) gearAxisRaw

                            axisDot =
                                abs (Vec3.dot compAxis gearAxis)
                        in
                        if axisDot < 0.94 then
                            []

                        else
                            gearHoleOffsets gear.spec.partFile
                                |> List.filterMap
                                    (\( hx, hy ) ->
                                        let
                                            holeWorld =
                                                Mat4.transform gear.worldMatrix (Vec3.vec3 hx hy 0)

                                            holeCenter =
                                                toYUpPoint holeWorld

                                            lineDist =
                                                pointToLineDistance compCenter holeCenter gearAxis

                                            axialOffset =
                                                abs (Vec3.dot (Vec3.sub compCenter holeCenter) gearAxis)
                                        in
                                        if lineDist <= 2.5 && axialOffset <= 40 then
                                            Just ( ( lineDist, axialOffset ), gear.id )

                                        else
                                            Nothing
                                    )
                    )
    in
    candidates
        |> List.sortBy Tuple.first
        |> List.head
        |> Maybe.map Tuple.second


{-| Determine whether a top-level LDraw line is co-axial with any detected
gear. Returns the id of the closest qualifying gear, or Nothing.

Used in `finishLoading` to decide which non-gear, non-component parts should
rotate with a gear rather than being baked into the static scene.

-}
topLevelCoaxialGear : PartCache -> List DrivenAxle -> LDrawLine -> List GearInstance -> Maybe GearId
topLevelCoaxialGear cache drivenAxles line gears =
    case line of
        SubFileRef ref ->
            let
                pos =
                    toYUpPoint (Mat4.transform ref.transform (Vec3.vec3 0 0 0))

                connectorPoints =
                    connectorLocalPoints cache ref.file
                        |> List.map (\local -> toYUpPoint (Mat4.transform ref.transform local))

                samplePoints =
                    pos :: connectorPoints

                axleMatch =
                    drivenAxles
                        |> List.filterMap
                            (\axle ->
                                bestLineMatchForPoints samplePoints axle.center axle.axis
                                    |> Maybe.andThen
                                        (\( lineDist, axialOffset ) ->
                                            if lineDist <= 6 && axialOffset <= 240 then
                                                Just ( lineDist, axialOffset, axle.drivingGearId )

                                            else
                                                Nothing
                                        )
                            )
                        |> List.sortBy (\( lineDist, axialOffset, _ ) -> ( lineDist, axialOffset ))
                        |> List.head
                        |> Maybe.map (\( _, _, gearId ) -> gearId)

                gearMatch =
                    gears
                        |> List.concatMap
                            (\gear ->
                                let
                                    gearCenter =
                                        toYUpPoint gear.worldPosition

                                    gearAxisEnd =
                                        toYUpPoint (Mat4.transform gear.worldMatrix (Vec3.vec3 0 0 1))

                                    gearAxisRaw =
                                        Vec3.sub gearAxisEnd gearCenter

                                    gearAxisLen =
                                        Vec3.length gearAxisRaw

                                    gearAxis =
                                        if gearAxisLen < 1.0e-6 then
                                            Vec3.vec3 0 1 0

                                        else
                                            Vec3.scale (1 / gearAxisLen) gearAxisRaw
                                in
                                gearHoleOffsets gear.spec.partFile
                                    |> List.filterMap
                                        (\( hx, hy ) ->
                                            let
                                                holeWorld =
                                                    Mat4.transform gear.worldMatrix (Vec3.vec3 hx hy 0)

                                                holeCenter =
                                                    toYUpPoint holeWorld
                                            in
                                            bestLineMatchForPoints samplePoints holeCenter gearAxis
                                                |> Maybe.andThen
                                                    (\( lineDist, axialOffset ) ->
                                                        if lineDist <= 2.5 && axialOffset <= 120 then
                                                            Just ( lineDist, axialOffset, gear.id )

                                                        else
                                                            Nothing
                                                    )
                                        )
                            )
                        |> List.sortBy (\( lineDist, axialOffset, _ ) -> ( lineDist, axialOffset ))
                        |> List.head
                        |> Maybe.map (\( _, _, gearId ) -> gearId)
            in
            case axleMatch of
                Just gearId ->
                    Just gearId

                Nothing ->
                    gearMatch

        _ ->
            Nothing


gearHoleOffsets : String -> List ( Float, Float )
gearHoleOffsets partFile =
    let
        clean =
            partFile
                |> String.replace "\\" "/"
                |> String.replace "parts/" ""
                |> String.replace ".dat" ""
                |> String.toLower
    in
    case clean of
        "3648" ->
            ( 0, 0 ) :: pinHoles24T

        "3648a" ->
            ( 0, 0 ) :: pinHoles24T

        "3648b" ->
            ( 0, 0 ) :: pinHoles24T

        "3649" ->
            ( 0, 0 ) :: (axleHoles40T ++ pinHoles40T)

        _ ->
            [ ( 0, 0 ) ]


pinHoles24T : List ( Float, Float )
pinHoles24T =
    [ ( 10, 10 ), ( -10, 10 ), ( -10, -10 ), ( 10, -10 ) ]


axleHoles40T : List ( Float, Float )
axleHoles40T =
    [ ( 0, 20 ), ( 0, -20 ), ( 20, 0 ), ( -20, 0 ) ]


pinHoles40T : List ( Float, Float )
pinHoles40T =
    [ ( 10, 10 )
    , ( -10, 10 )
    , ( -10, -10 )
    , ( 10, -10 )
    , ( 10, 30 )
    , ( -10, 30 )
    , ( 10, -30 )
    , ( -10, -30 )
    , ( 30, 10 )
    , ( -30, 10 )
    , ( 30, -10 )
    , ( -30, -10 )
    ]


bestLineMatchForPoints : List Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Maybe ( Float, Float )
bestLineMatchForPoints points lineOrigin lineDir =
    points
        |> List.map
            (\point ->
                let
                    lineDist =
                        pointToLineDistance point lineOrigin lineDir

                    axialOffset =
                        abs (Vec3.dot (Vec3.sub point lineOrigin) lineDir)
                in
                ( lineDist, axialOffset )
            )
        |> List.sortBy (\( lineDist, axialOffset ) -> ( lineDist, axialOffset ))
        |> List.head


{-| Build rotating `ComponentMeshRender` entries for parts that are co-axial
with a detected gear but are not in `Components.defaultSpecs`.

These parts are excluded from the static scene geometry worker and rendered
with the same rotation angle as their driving gear.

-}
buildCoaxialMeshRenders :
    PartCache
    -> List GearInstance
    -> List { file : String, color : Int, transform : Mat4.Mat4, drivingGearId : GearId }
    -> List ComponentMeshRender
buildCoaxialMeshRenders cache gears parts =
    List.filterMap
        (\part ->
            case Dict.get part.file cache of
                Just (Loaded lines) ->
                    let
                        geom =
                            Geometry.flatten lines cache part.color part.transform

                        center =
                            toYUpPoint (Mat4.transform part.transform (Vec3.vec3 0 0 0))

                        axisEnd =
                            toYUpPoint (Mat4.transform part.transform (Vec3.vec3 0 0 1))

                        axisRaw =
                            Vec3.sub axisEnd center

                        axisLen =
                            Vec3.length axisRaw

                        localAxis =
                            if axisLen < 1.0e-6 then
                                Vec3.vec3 0 1 0

                            else
                                canonicalizeAxis (Vec3.scale (1 / axisLen) axisRaw)

                        ( rotationCenter, axis ) =
                            case gearAxisInfoById part.drivingGearId gears of
                                Just info ->
                                    ( projectPointToLine center info.center info.axis
                                    , info.axis
                                    )

                                Nothing ->
                                    ( center, localAxis )
                    in
                    Just
                        { mesh = WebGL.triangles geom.triangles
                        , surface = geom.triangles
                        , edges = edgeMesh geom.lines
                        , center = center
                        , rotationCenter = rotationCenter
                        , axis = axis
                        , drivingGearId = Just part.drivingGearId
                        }

                _ ->
                    Nothing
        )
        parts


pointToLineDistance : Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Float
pointToLineDistance point lineOrigin lineDir =
    let
        offset =
            Vec3.sub point lineOrigin

        projectionLen =
            Vec3.dot offset lineDir

        projected =
            Vec3.scale projectionLen lineDir

        rejection =
            Vec3.sub offset projected
    in
    Vec3.length rejection


projectPointToLine : Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3
projectPointToLine point lineOrigin lineDir =
    let
        offset =
            Vec3.sub point lineOrigin

        projectionLen =
            Vec3.dot offset lineDir
    in
    Vec3.add lineOrigin (Vec3.scale projectionLen lineDir)


type alias GearAxisInfo =
    { center : Vec3.Vec3
    , axis : Vec3.Vec3
    }


type alias DrivenAxle =
    { center : Vec3.Vec3
    , axis : Vec3.Vec3
    , drivingGearId : GearId
    , file : String
    , transform : Mat4.Mat4
    }


buildDrivenAxles : PartCache -> List Components.ComponentInstance -> List GearInstance -> List DrivenAxle
buildDrivenAxles cache components gears =
    components
        |> List.filter (\component -> component.kind == Components.AxleLike)
        |> List.filterMap
            (\component ->
                componentDrivingGear cache component gears
                    |> Maybe.map
                        (\gearId ->
                            { center = toYUpPoint component.worldPosition
                            , axis =
                                alignToGear (Just gearId) gears (inferComponentAxis cache component)
                            , drivingGearId = gearId
                            , file = component.partFile
                            , transform = component.worldMatrix
                            }
                        )
            )


connectorLocalPoints : PartCache -> String -> List Vec3.Vec3
connectorLocalPoints cache partFile =
    case Dict.get partFile cache of
        Just (Loaded lines) ->
            lines
                |> List.filterMap
                    (\line ->
                        case line of
                            SubFileRef subRef ->
                                if isRotationalConnectorReference subRef.file then
                                    Just (Mat4.transform subRef.transform (Vec3.vec3 0 0 0))

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )

        _ ->
            []


isRotationalConnectorReference : String -> Bool
isRotationalConnectorReference file =
    let
        lower =
            String.toLower file
    in
    List.any
        (\needle -> String.contains needle lower)
        [ "axlehol"
        , "axleend"
        , "axle.dat"
        ]


gearAxisInfoById : GearId -> List GearInstance -> Maybe GearAxisInfo
gearAxisInfoById targetId gears =
    gears
        |> List.filter (\gear -> gear.id == targetId)
        |> List.head
        |> Maybe.map
            (\gear ->
                let
                    center =
                        toYUpPoint gear.worldPosition

                    axisEnd =
                        toYUpPoint (Mat4.transform gear.worldMatrix (Vec3.vec3 0 0 1))

                    rawAxis =
                        Vec3.sub axisEnd center

                    axisLen =
                        Vec3.length rawAxis

                    axis =
                        if axisLen < 1.0e-6 then
                            Vec3.vec3 0 1 0

                        else
                            Vec3.scale (1 / axisLen) rawAxis
                in
                { center = center, axis = axis }
            )


{-| The axis a driven part turns about, oriented like its driving gear's true
axis. Turning about `-a` by `-θ` is the same motion, so a part whose own axis
points against its gear gets the flipped axis and the gear's angle unchanged.
Without a driving gear the axis is returned as it is.
-}
alignToGear : Maybe GearId -> List GearInstance -> Vec3.Vec3 -> Vec3.Vec3
alignToGear drivingGear gears axis =
    case Maybe.andThen (\gearId -> gearAxisInfoById gearId gears) drivingGear of
        Just info ->
            if Vec3.dot axis info.axis < 0 then
                Vec3.scale -1 axis

            else
                axis

        Nothing ->
            axis


inferComponentAxis : PartCache -> Components.ComponentInstance -> Vec3.Vec3
inferComponentAxis cache component =
    let
        localAxis =
            inferPartLocalAxis cache component.partFile

        origin =
            Mat4.transform component.worldMatrix (Vec3.vec3 0 0 0)

        axisEnd =
            Mat4.transform component.worldMatrix localAxis

        raw =
            Vec3.sub axisEnd origin

        len =
            Vec3.length raw
    in
    if len < 1.0e-6 then
        Vec3.vec3 0 1 0

    else
        canonicalizeAxis (Vec3.scale (1 / len) raw)


inferPartLocalAxis : PartCache -> String -> Vec3.Vec3
inferPartLocalAxis cache partFile =
    case Dict.get partFile cache of
        Just (Loaded lines) ->
            let
                geom =
                    Geometry.flatten lines cache 15 Mat4.identity

                points =
                    (geom.triangles
                        |> List.concatMap (\( a, b, c ) -> [ a.position, b.position, c.position ])
                    )
                        ++ (geom.lines
                                |> List.concatMap (\( p1, p2 ) -> [ p1, p2 ])
                           )

                initialBounds =
                    { minX = 1 / 0
                    , maxX = -(1 / 0)
                    , minY = 1 / 0
                    , maxY = -(1 / 0)
                    , minZ = 1 / 0
                    , maxZ = -(1 / 0)
                    }

                bounds =
                    List.foldl
                        (\p acc ->
                            { minX = min acc.minX (Vec3.getX p)
                            , maxX = max acc.maxX (Vec3.getX p)
                            , minY = min acc.minY (Vec3.getY p)
                            , maxY = max acc.maxY (Vec3.getY p)
                            , minZ = min acc.minZ (Vec3.getZ p)
                            , maxZ = max acc.maxZ (Vec3.getZ p)
                            }
                        )
                        initialBounds
                        points

                extentX =
                    bounds.maxX - bounds.minX

                extentY =
                    bounds.maxY - bounds.minY

                extentZ =
                    bounds.maxZ - bounds.minZ
            in
            if extentX >= extentY && extentX >= extentZ then
                Vec3.vec3 1 0 0

            else if extentY >= extentZ then
                Vec3.vec3 0 1 0

            else
                Vec3.vec3 0 0 1

        _ ->
            Vec3.vec3 0 0 1
