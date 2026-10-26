module Studio.StepsTest exposing (suite)

{-| Tests for the move steps (`Core.setNudgeSteps`), camera-relative nudges,
glued parts that refuse a move, and the Tab selection order (`Core.selectNeighbour`).

Nudges are checked in render space (Y up, the camera's frame): the camera at
azimuth `a` looks along `(-sin a, 0, -cos a)`, and its right is `(cos a, 0, -sin a)`.
The expected move is that direction snapped to the nearest world X or Z axis.

-}

import Dict
import Expect
import LDraw.Document as Document
import LDraw.Parser as Parser
import LDraw.Resolve exposing (PartStatus(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set
import Studio.Core as Core
import Studio.Shortcuts as StudioShortcuts
import Test exposing (Test, describe, test)
import Time
import Viewport


suite : Test
suite =
    describe "Move steps, nudges and Tab selection"
        [ stepSuite
        , cameraSuite
        , glueSuite
        , tabSuite
        ]


stepSuite : Test
stepSuite =
    describe "setNudgeSteps"
        [ test "Stud steps: 20 LDU sideways, 24 LDU up and down" <|
            \_ ->
                let
                    model =
                        bricks [ vec3 0 -24 0 ] |> Core.setNudgeSteps { horizontal = 20, vertical = 24 }
                in
                Expect.all
                    [ \_ -> Expect.equal (vec3 20 0 0) (moveOf StudioShortcuts.Right False model)
                    , \_ -> Expect.equal (vec3 0 -24 0) (moveOf StudioShortcuts.Up False model)
                    , \_ -> Expect.equal (vec3 0 0 20) (moveOf StudioShortcuts.Away False model)
                    ]
                    ()
        , test "half-stud steps: 10 LDU sideways, 8 LDU up and down" <|
            \_ ->
                let
                    model =
                        bricks [ vec3 0 -24 0 ] |> Core.setNudgeSteps { horizontal = 10, vertical = 8 }
                in
                Expect.all
                    [ \_ -> Expect.equal (vec3 10 0 0) (moveOf StudioShortcuts.Right False model)
                    , \_ -> Expect.equal (vec3 0 -8 0) (moveOf StudioShortcuts.Up False model)
                    , \_ -> Expect.equal (vec3 0 0 10) (moveOf StudioShortcuts.Away False model)
                    ]
                    ()
        , test "bit steps: 1 LDU on every axis" <|
            \_ ->
                let
                    model =
                        bricks [ vec3 0 -24 0 ] |> Core.setNudgeSteps { horizontal = 1, vertical = 1 }
                in
                Expect.all
                    [ \_ -> Expect.equal (vec3 1 0 0) (moveOf StudioShortcuts.Right False model)
                    , \_ -> Expect.equal (vec3 0 -1 0) (moveOf StudioShortcuts.Up False model)
                    ]
                    ()
        , test "Shift (a fine nudge) forces 1 LDU whatever the steps are" <|
            \_ ->
                let
                    model =
                        bricks [ vec3 0 -24 0 ] |> Core.setNudgeSteps { horizontal = 20, vertical = 24 }
                in
                Expect.all
                    [ \_ -> Expect.equal (vec3 1 0 0) (moveOf StudioShortcuts.Right True model)
                    , \_ -> Expect.equal (vec3 0 -1 0) (moveOf StudioShortcuts.Up True model)
                    ]
                    ()
        , test "without setNudgeSteps the nudge follows the grid (8 LDU vertical at most)" <|
            \_ ->
                let
                    model =
                        bricks [ vec3 0 -24 0 ] |> Core.setGridStep 20
                in
                Expect.all
                    [ \_ -> Expect.equal (vec3 20 0 0) (moveOf StudioShortcuts.Right False model)
                    , \_ -> Expect.equal (vec3 0 -8 0) (moveOf StudioShortcuts.Up False model)
                    ]
                    ()
        ]


cameraSuite : Test
cameraSuite =
    describe "camera-relative nudges"
        (List.map
            (\degrees_ ->
                test ("stud steps follow the camera at " ++ String.fromFloat degrees_ ++ " degrees") <|
                    \_ ->
                        let
                            azimuth =
                                degrees degrees_

                            model =
                                bricks [ vec3 0 -24 0 ]
                                    |> Core.setNudgeSteps { horizontal = 20, vertical = 24 }
                                    |> atAzimuth azimuth

                            right =
                                vec3 (cos azimuth) 0 -(sin azimuth)

                            away =
                                vec3 -(sin azimuth) 0 -(cos azimuth)

                            expected direction =
                                snapToAxis 20 direction
                        in
                        Expect.all
                            [ \_ -> Expect.equal (expected away) (renderMove StudioShortcuts.Away model)
                            , \_ -> Expect.equal (expected (Vec3.negate away)) (renderMove StudioShortcuts.Toward model)
                            , \_ -> Expect.equal (expected right) (renderMove StudioShortcuts.Right model)
                            , \_ -> Expect.equal (expected (Vec3.negate right)) (renderMove StudioShortcuts.Left model)
                            ]
                            ()
            )
            [ 0, 90, 180, 270, 44, -44, 224, 46 ]
        )


glueSuite : Test
glueSuite =
    describe "glued parts"
        [ test "a glued part does not move on a nudge" <|
            \_ ->
                let
                    model =
                        bricks [ vec3 0 -24 0 ]
                            |> Core.setNudgeSteps { horizontal = 20, vertical = 24 }
                            |> selectFirst
                            |> Core.setGlued True
                            |> (\( m, _, _ ) -> m)
                in
                Expect.all
                    [ \_ -> Expect.equal (vec3 0 -24 0) (positionOf 0 model)
                    , \_ -> Expect.equal (vec3 0 -24 0) (positionOf 0 (nudged StudioShortcuts.Right model))
                    , \_ -> Expect.equal (vec3 0 -24 0) (positionOf 0 (nudged StudioShortcuts.Up model))
                    ]
                    ()
        ]


tabSuite : Test
tabSuite =
    describe "selectNeighbour (Tab)"
        [ test "the order is bottom to top, then back to front, then left to right" <|
            \_ ->
                -- Added as C, A, B: the order must not depend on that.
                let
                    model =
                        bricks [ vec3 0 -24 40, vec3 0 0 0, vec3 40 -24 0 ]
                in
                -- A is on the ground (Y 0), B and C sit one brick up; B has the smaller Z.
                Expect.equal [ vec3 0 0 0, vec3 40 -24 0, vec3 0 -24 40 ]
                    (List.map (\id -> positionOf id model) (Core.partOrder model))
        , test "Tab walks the order and wraps round; Shift+Tab goes backward" <|
            \_ ->
                let
                    model =
                        bricks [ vec3 0 -24 40, vec3 0 0 0, vec3 40 -24 0 ]

                    tab backwards m =
                        Core.selectNeighbour { backwards = backwards } m |> (\( next, _, _ ) -> next)

                    selectedPosition m =
                        Set.toList m.selection
                            |> List.head
                            |> Maybe.map (\id -> positionOf id m)
                in
                Expect.all
                    [ \_ -> Expect.equal (Just (vec3 0 0 0)) (selectedPosition (tab False model))
                    , \_ -> Expect.equal (Just (vec3 40 -24 0)) (selectedPosition (tab False (tab False model)))
                    , \_ -> Expect.equal (Just (vec3 0 -24 40)) (selectedPosition (tab False (tab False (tab False model))))
                    , \_ -> Expect.equal (Just (vec3 0 0 0)) (selectedPosition (tab False (tab False (tab False (tab False model)))))
                    , \_ -> Expect.equal (Just (vec3 0 -24 40)) (selectedPosition (tab True model))
                    ]
                    ()
        , test "Tab selects the part explicitly, as a click does, and emits a selection change" <|
            \_ ->
                let
                    ( next, _, events ) =
                        Core.selectNeighbour { backwards = False } (bricks [ vec3 0 -24 0 ])
                in
                Expect.all
                    [ \_ -> Expect.equal (Set.size next.selection) 1
                    , \_ -> Expect.equal [ Core.SelectionChanged next.selection ] events
                    ]
                    ()
        , test "Tab glides the camera onto the selected part" <|
            \_ ->
                let
                    far =
                        bricks [ vec3 0 -24 0, vec3 400 -24 0 ]

                    ( tabbed, _, _ ) =
                        Core.selectNeighbour { backwards = False } far

                    settled =
                        tabbed
                            |> Core.update (Core.ViewportMsg (Viewport.AnimationFrame (Time.millisToPosix 0)))
                            |> (\( m, _, _ ) -> m)
                            |> Core.update (Core.ViewportMsg (Viewport.AnimationFrame (Time.millisToPosix 1000)))
                            |> (\( m, _, _ ) -> m)
                in
                -- The first part is at X 0; the target has glided off the origin's
                -- default framing towards it.
                Expect.all
                    [ \_ -> Expect.equal False (settled.viewport.camera.target == far.viewport.camera.target)
                    , \_ -> Expect.lessThan 60 (Vec3.distance settled.viewport.camera.target (vec3 0 0 0))
                    ]
                    ()
        , test "Tab with nothing in the build does nothing" <|
            \_ ->
                let
                    ( next, _, events ) =
                        Core.selectNeighbour { backwards = False } (bricks [])
                in
                Expect.all
                    [ \_ -> Expect.equal Set.empty next.selection
                    , \_ -> Expect.equal [] events
                    ]
                    ()
        , test "a double-click reports its canvas position as DoubleClickedAt" <|
            \_ ->
                let
                    ( _, _, events ) =
                        Core.update (Core.ViewportMsg (Viewport.DoubleClick 12 34)) (bricks [ vec3 0 -24 0 ])
                in
                Expect.equal True (List.member (Core.DoubleClickedAt 12 34) events)
        , test "a one-finger tap reports its canvas position as TappedAt" <|
            \_ ->
                let
                    tapped =
                        bricks [ vec3 0 -24 0 ]
                            |> Core.update (Core.ViewportMsg (Viewport.TouchStart [ { id = 1, x = 15, y = 25 } ]))
                            |> (\( m, _, _ ) -> m)
                            |> Core.update (Core.ViewportMsg (Viewport.TouchEnd []))
                            |> (\( _, _, events ) -> events)
                in
                Expect.equal True (List.member (Core.TappedAt 15 25) tapped)
        ]



-- Helpers


{-| A build of 3001 bricks at these LDraw origins, with real (tiny) geometry, in
the order given (so the first is id 0).
-}
bricks : List Vec3 -> Core.Model
bricks positions =
    let
        doc =
            List.foldl
                (\pos d -> Document.addPlacement "" { part = "3001.dat", color = 4, matrix = Mat4.makeTranslate pos } d |> Tuple.first)
                Document.empty
                positions

        ( model0, _ ) =
            Core.init
                { ldrawBase = "/ldraw"
                , ldrawFallbackBase = ""
                , defaultModelUrl = ""
                , initialHash = ""
                , supportsPointerEvents = True
                , document = Document.empty
                , gridStep = Just 20
                , rotateStep = Just 90
                , snapDistance = Just 24.0
                , collisionMode = Nothing
                }

        geometry =
            Parser.parseFile "4 16 -10 0 -10 10 0 -10 10 0 10 -10 0 10\n4 16 -10 24 -10 10 24 -10 10 24 10 -10 24 10"

        vp =
            model0.viewport

        ( loadedViewport, _, _ ) =
            Viewport.loadDocument doc { vp | partCache = Dict.insert "3001.dat" (Loaded geometry) vp.partCache }
    in
    { model0 | viewport = loadedViewport, document = doc }


{-| The camera at `azimuth` (radians), as a viewer's orbit leaves it.
-}
atAzimuth : Float -> Core.Model -> Core.Model
atAzimuth azimuth model =
    let
        vp =
            model.viewport

        cam =
            vp.camera
    in
    { model | viewport = { vp | camera = { cam | azimuth = azimuth } } }


selectFirst : Core.Model -> Core.Model
selectFirst model =
    case Core.partOrder model of
        id :: _ ->
            Core.update (Core.SelectOnly id) model |> (\( m, _, _ ) -> m)

        [] ->
            model


{-| The LDraw origin of placement `id`.
-}
positionOf : Int -> Core.Model -> Vec3
positionOf id model =
    Core.getPlacements model
        |> List.filter (\p -> p.id == id)
        |> List.head
        |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))
        |> Maybe.withDefault (vec3 0 0 0)


{-| Select the first placement and nudge it: its LDraw origin afterwards minus
the one before, which is the LDraw move.
-}
nudged : StudioShortcuts.NudgeDirection -> Core.Model -> Core.Model
nudged direction model =
    Core.update (Core.NudgeSelection (StudioShortcuts.Nudge { direction = direction, fine = False })) (selectFirst model)
        |> (\( m, _, _ ) -> m)


{-| The LDraw move one nudge makes to the first brick, `fine` for Shift.
-}
moveOf : StudioShortcuts.NudgeDirection -> Bool -> Core.Model -> Vec3
moveOf direction fine model =
    let
        before =
            positionOf 0 (selectFirst model)

        after =
            Core.update (Core.NudgeSelection (StudioShortcuts.Nudge { direction = direction, fine = fine })) (selectFirst model)
                |> (\( m, _, _ ) -> m)
                |> positionOf 0
    in
    Vec3.sub after before


{-| The same move, in render space (Y up, the camera's frame).
-}
renderMove : StudioShortcuts.NudgeDirection -> Core.Model -> Vec3
renderMove direction model =
    toRender (moveOf direction False model)


{-| LDraw to render space: Y is up, and Z points the other way.
-}
toRender : Vec3 -> Vec3
toRender v =
    vec3 (Vec3.getX v) -(Vec3.getY v) -(Vec3.getZ v)


{-| The move a direction `(dx, 0, dz)` snaps to: its larger ground component
on the nearest axis, `step` LDU long.
-}
snapToAxis : Float -> Vec3 -> Vec3
snapToAxis step direction =
    let
        dx =
            Vec3.getX direction

        dz =
            Vec3.getZ direction

        sign v =
            if v < 0 then
                -1

            else
                1
    in
    if abs dx >= abs dz then
        vec3 (step * sign dx) 0 0

    else
        vec3 0 0 (step * sign dz)
