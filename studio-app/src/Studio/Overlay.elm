module Studio.Overlay exposing (Highlights, entities)

{-| What the studio draws on top of the model: selection outlines and the move
gizmo.

Both are drawn without a depth test, so a selected brick can be found even when
other parts hide it. Everything is in render space (Y up); document-space
positions go through `Studio.Gizmo.toRender` first.

-}

import Dict
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Render.Camera as Camera exposing (Camera)
import Render.Instanced exposing (InstancedScene)
import Set exposing (Set)
import Studio.Edit exposing (Axis(..))
import Studio.Gizmo as Gizmo exposing (ArcHandle, Handle)
import Studio.Shader as Shader exposing (Vertex)
import WebGL
import WebGL.Settings.DepthTest as DepthTest


{-| Which placements to outline, and in which colour: the selection in yellow,
gears that mesh in green, gears that cannot turn in orange.
-}
type alias Highlights =
    { selection : Set Int
    , meshed : Set Int
    , isolated : Set Int
    }


{-| The outlines for `highlights`, and the gizmo handles if there are any.
-}
entities : Camera -> Float -> Maybe InstancedScene -> Highlights -> List Handle -> List ArcHandle -> List WebGL.Entity
entities camera aspect scene highlights handles arcHandles =
    let
        uniforms color =
            { viewMatrix = Camera.viewMatrix camera
            , projectionMatrix = Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)
            , color = color
            }

        draw mesh color =
            WebGL.entityWith
                [ DepthTest.always { write = False, near = 0, far = 1 } ]
                Shader.vertexShader
                Shader.fragmentShader
                mesh
                (uniforms color)

        outline sources color =
            case scene of
                Just s ->
                    case selectedBoxes s sources of
                        [] ->
                            []

                        boxes ->
                            [ draw (WebGL.lines (List.concatMap boxEdges boxes)) color ]

                Nothing ->
                    []
    in
    outline highlights.meshed meshedColor
        ++ outline highlights.isolated isolatedColor
        ++ outline highlights.selection outlineColor
        ++ List.map (\h -> draw (WebGL.triangles (handleTriangles camera h)) (axisColor h.axis)) handles
        ++ List.map (\ah -> draw (WebGL.lines (arcLines ah)) (arcColor ah)) arcHandles


outlineColor : Vec4
outlineColor =
    vec4 0.98 0.78 0.04 1


meshedColor : Vec4
meshedColor =
    vec4 0.15 0.65 0.25 1


isolatedColor : Vec4
isolatedColor =
    vec4 0.95 0.45 0.1 1


axisColor : Axis -> Vec4
axisColor axis =
    case axis of
        AxisX ->
            vec4 0.85 0.15 0.12 1

        AxisY ->
            vec4 0.15 0.65 0.25 1

        AxisZ ->
            vec4 0.15 0.35 0.85 1


arcColor : ArcHandle -> Vec4
arcColor handle =
    case handle.axisKind of
        Just axis ->
            axisColor axis

        Nothing ->
            vec4 0.1 0.75 0.85 1


arcLines : ArcHandle -> List ( Vertex, Vertex )
arcLines handle =
    let
        renderCenter =
            Gizmo.toRender handle.center

        renderAxis =
            Gizmo.toRender handle.axis

        ( u, v ) =
            Gizmo.orthogonalBasis renderAxis

        segments =
            32

        step =
            2 * pi / toFloat segments

        circlePoint i =
            let
                angle =
                    toFloat i * step

                offset =
                    Vec3.add (Vec3.scale (handle.radius * cos angle) u) (Vec3.scale (handle.radius * sin angle) v)
            in
            Vertex (Vec3.add renderCenter offset)

        indices =
            List.range 0 (segments - 1)
    in
    List.map (\i -> ( circlePoint i, circlePoint (modBy segments (i + 1)) )) indices


{-| The eight corners of each selected placement's bounding box, in render space.
At most `maxOutlines` are drawn so selecting everything in a city-scale model
does not build a mesh per brick on every frame.
-}
selectedBoxes : InstancedScene -> Set Int -> List (List Vec3)
selectedBoxes scene selection =
    scene.placements
        |> List.filter (\p -> Set.member p.source selection)
        |> List.take maxOutlines
        |> List.filterMap
            (\p ->
                Dict.get p.part scene.parts
                    |> Maybe.map
                        (\mesh ->
                            corners mesh.boundsMin mesh.boundsMax
                                |> List.map (Mat4.transform p.matrix)
                        )
            )


maxOutlines : Int
maxOutlines =
    500


{-| Corners in the order `boxEdges` expects: bit 0 is x, bit 1 is y, bit 2 is z.
-}
corners : Vec3 -> Vec3 -> List Vec3
corners lo hi =
    List.range 0 7
        |> List.map
            (\i ->
                vec3
                    (pick (modBy 2 i == 1) (Vec3.getX lo) (Vec3.getX hi))
                    (pick (modBy 2 (i // 2) == 1) (Vec3.getY lo) (Vec3.getY hi))
                    (pick (modBy 2 (i // 4) == 1) (Vec3.getZ lo) (Vec3.getZ hi))
            )


pick : Bool -> Float -> Float -> Float
pick high lo hi =
    if high then
        hi

    else
        lo


{-| The twelve edges of a box given its corners, as vertex pairs.
-}
boxEdges : List Vec3 -> List ( Vertex, Vertex )
boxEdges box =
    let
        at i =
            List.drop i box |> List.head |> Maybe.withDefault (vec3 0 0 0)

        edge a b =
            ( { position = at a }, { position = at b } )
    in
    [ edge 0 1
    , edge 2 3
    , edge 4 5
    , edge 6 7
    , edge 0 2
    , edge 1 3
    , edge 4 6
    , edge 5 7
    , edge 0 4
    , edge 1 5
    , edge 2 6
    , edge 3 7
    ]


{-| A handle as a box from the centre to the tip, thick enough to see and to hit.
-}
handleTriangles : Camera -> Handle -> List ( Vertex, Vertex, Vertex )
handleTriangles _ handle =
    let
        a =
            Gizmo.toRender handle.origin

        b =
            Gizmo.toRender handle.tip

        w =
            handle.length * 0.035

        lo =
            vec3 (min (Vec3.getX a) (Vec3.getX b) - w) (min (Vec3.getY a) (Vec3.getY b) - w) (min (Vec3.getZ a) (Vec3.getZ b) - w)

        hi =
            vec3 (max (Vec3.getX a) (Vec3.getX b) + w) (max (Vec3.getY a) (Vec3.getY b) + w) (max (Vec3.getZ a) (Vec3.getZ b) + w)

        box =
            corners lo hi

        at i =
            List.drop i box |> List.head |> Maybe.withDefault (vec3 0 0 0)

        quad p q r s =
            [ ( { position = at p }, { position = at q }, { position = at r } )
            , ( { position = at p }, { position = at r }, { position = at s } )
            ]
    in
    List.concat
        [ quad 0 1 3 2
        , quad 4 5 7 6
        , quad 0 1 5 4
        , quad 2 3 7 6
        , quad 0 2 6 4
        , quad 1 3 7 5
        ]
