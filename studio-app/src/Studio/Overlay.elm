module Studio.Overlay exposing (Highlights, entities)

{-| What the studio draws on top of the model: selection outlines and the move
gizmo.

Both are drawn without a depth test, so a selected brick can be found even when
other parts hide it. Everything is in render space (Y up); document-space
positions go through `Studio.Gizmo.toRender` first.

-}

import Dict
import LDraw.Connectivity as Connectivity exposing (SnapGuide(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Render.Camera as Camera exposing (Camera)
import Render.Instanced exposing (InstancedScene)
import Set exposing (Set)
import Studio.Edit exposing (Axis(..))
import Studio.Gizmo as Gizmo exposing (Handle)
import Studio.Shader as Shader exposing (Vertex)
import WebGL
import WebGL.Settings.DepthTest as DepthTest


{-| Which placements to outline, and in which colour: the selection in yellow,
gears that mesh in green, gears that cannot turn in orange, collisions in red,
and active magnetic snap guides.
-}
type alias Highlights =
    { selection : Set Int
    , meshed : Set Int
    , isolated : Set Int
    , colliding : Set Int
    , snapGuide : Maybe SnapGuide
    }


{-| The outlines for `highlights`, snap guides, and the gizmo handles if there are any.
-}
entities : Camera -> Float -> Maybe InstancedScene -> Highlights -> List Handle -> List WebGL.Entity
entities camera aspect scene highlights handles =
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
        ++ outline highlights.colliding collidingColor
        ++ outline highlights.selection outlineColor
        ++ snapGuideEntities draw highlights.snapGuide
        ++ List.map (\h -> draw (WebGL.triangles (handleTriangles camera h)) (axisColor h.axis)) handles


snapGuideEntities : (WebGL.Mesh Vertex -> Vec4 -> WebGL.Entity) -> Maybe SnapGuide -> List WebGL.Entity
snapGuideEntities draw maybeGuide =
    case maybeGuide of
        Just (StudRingGuide pos normal isColliding) ->
            let
                color =
                    if isColliding then
                        collidingColor

                    else
                        snapValidColor

                rPos =
                    Gizmo.toRender pos

                rNorm =
                    let
                        rn =
                            Gizmo.toRender normal

                        len =
                            Vec3.length rn
                    in
                    if len < 1.0e-6 then
                        vec3 0 1 0

                    else
                        Vec3.scale (1 / len) rn

                rTangent =
                    let
                        hint =
                            if abs (Vec3.getX rNorm) < 0.9 then
                                vec3 1 0 0

                            else
                                vec3 0 0 1

                        proj =
                            Vec3.sub hint (Vec3.scale (Vec3.dot hint rNorm) rNorm)

                        pLen =
                            Vec3.length proj
                    in
                    if pLen < 1.0e-6 then
                        vec3 1 0 0

                    else
                        Vec3.scale (1 / pLen) proj

                rBitangent =
                    Vec3.cross rNorm rTangent

                radius =
                    7.5

                segments =
                    16

                step =
                    2 * pi / toFloat segments

                circlePoint i =
                    let
                        angle =
                            toFloat i * step
                    in
                    Vec3.add
                        rPos
                        (Vec3.add
                            (Vec3.scale (radius * cos angle) rTangent)
                            (Vec3.scale (radius * sin angle) rBitangent)
                        )

                edges =
                    List.range 0 (segments - 1)
                        |> List.map
                            (\i ->
                                ( { position = circlePoint i }
                                , { position = circlePoint (modBy segments (i + 1)) }
                                )
                            )
            in
            [ draw (WebGL.lines edges) color ]

        Just (AxleAxisGuide pos axis isColliding) ->
            let
                color =
                    if isColliding then
                        collidingColor

                    else
                        axleColor

                rPos =
                    Gizmo.toRender pos

                rAxis =
                    let
                        ra =
                            Gizmo.toRender axis

                        len =
                            Vec3.length ra
                    in
                    if len < 1.0e-6 then
                        vec3 0 0 1

                    else
                        Vec3.scale (1 / len) ra

                halfLen =
                    25.0

                p1 =
                    Vec3.add rPos (Vec3.scale -halfLen rAxis)

                p2 =
                    Vec3.add rPos (Vec3.scale halfLen rAxis)
            in
            [ draw (WebGL.lines [ ( { position = p1 }, { position = p2 } ) ]) color ]

        Just (ClipLockGuide pos _ isColliding) ->
            let
                color =
                    if isColliding then
                        collidingColor

                    else
                        clipColor

                rPos =
                    Gizmo.toRender pos

                s =
                    6.0

                edges =
                    [ ( { position = Vec3.add rPos (vec3 -s 0 0) }
                      , { position = Vec3.add rPos (vec3 0 s 0) }
                      )
                    , ( { position = Vec3.add rPos (vec3 0 s 0) }
                      , { position = Vec3.add rPos (vec3 s 0 0) }
                      )
                    , ( { position = Vec3.add rPos (vec3 s 0 0) }
                      , { position = Vec3.add rPos (vec3 0 -s 0) }
                      )
                    , ( { position = Vec3.add rPos (vec3 0 -s 0) }
                      , { position = Vec3.add rPos (vec3 -s 0 0) }
                      )
                    ]
            in
            [ draw (WebGL.lines edges) color ]

        Nothing ->
            []


snapValidColor : Vec4
snapValidColor =
    vec4 0.15 0.85 0.3 1


collidingColor : Vec4
collidingColor =
    vec4 0.95 0.15 0.15 1


axleColor : Vec4
axleColor =
    vec4 0.1 0.8 0.95 1


clipColor : Vec4
clipColor =
    vec4 0.95 0.65 0.1 1


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


{-| A handle as an oriented box from the centre to the tip, thick enough to see and to hit.
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

        ab =
            Vec3.sub b a

        len =
            Vec3.length ab

        dir =
            if len < 1.0e-6 then
                vec3 1 0 0

            else
                Vec3.scale (1 / len) ab

        ref =
            if abs (Vec3.getY dir) < 0.9 then
                vec3 0 1 0

            else
                vec3 1 0 0

        u =
            Vec3.scale w (Vec3.normalize (Vec3.cross dir ref))

        v =
            Vec3.scale w (Vec3.normalize (Vec3.cross dir u))

        start =
            Vec3.sub a (Vec3.scale w dir)

        end =
            Vec3.add b (Vec3.scale w dir)

        corner base sU sV =
            Vec3.add base (Vec3.add (Vec3.scale sU u) (Vec3.scale sV v))

        box =
            [ corner start -1 -1
            , corner start 1 -1
            , corner start -1 1
            , corner start 1 1
            , corner end -1 -1
            , corner end 1 -1
            , corner end -1 1
            , corner end 1 1
            ]

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
