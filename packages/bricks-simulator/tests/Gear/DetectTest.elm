module Gear.DetectTest exposing (suite)

{-| Unit tests for gear detection and graph construction logic.
-}

import Array
import Dict
import Expect
import Gear.Detect exposing (buildGearGraph, extractGears)
import Gear.Types exposing (GearSpec)
import LDraw.Resolve exposing (PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import Test exposing (Test, describe, test)


-- ── Helpers ───────────────────────────────────────────────────────────────────


{-| Build a SubFileRef line at the given world translation.
-}
gearRef : String -> Float -> Float -> Float -> LDrawLine
gearRef file x y z =
    SubFileRef
        { color = 16
        , transform = Mat4.makeTranslate3 x y z
        , file = file
        }


emptyCache : LDraw.Resolve.PartCache
emptyCache =
    Dict.empty


spec8T : GearSpec
spec8T =
    { partFile = "3647.dat", teeth = 8, pitchRadius = 10.0 }


spec16T : GearSpec
spec16T =
    { partFile = "4019.dat", teeth = 16, pitchRadius = 20.0 }


specBevel20T : GearSpec
specBevel20T =
    { partFile = "32198.dat", teeth = 20, pitchRadius = 25.0 }


specCrown24T : GearSpec
specCrown24T =
    { partFile = "3650b.dat", teeth = 24, pitchRadius = 30.0 }


testGearSpecs : List GearSpec
testGearSpecs =
    [ spec8T
    , spec16T
    , specBevel20T
    , specCrown24T
    ]


-- ── Suite ─────────────────────────────────────────────────────────────────────


suite : Test
suite =
    describe "Gear.Detect"
        [ describe "extractGears"
            [ test "no SubFileRefs → empty list" <|
                \_ ->
                    extractGears testGearSpecs [] emptyCache
                        |> List.length
                        |> Expect.equal 0
            , test "non-gear SubFileRef is ignored" <|
                \_ ->
                    let
                        lines =
                            [ SubFileRef { color = 16, transform = Mat4.identity, file = "3001.dat" } ]
                    in
                    extractGears testGearSpecs lines emptyCache
                        |> List.length
                        |> Expect.equal 0
            , test "single 8T gear reference is detected" <|
                \_ ->
                    extractGears testGearSpecs [ gearRef "3647.dat" 0 0 0 ] emptyCache
                        |> List.length
                        |> Expect.equal 1
            , test "detected gear has correct spec" <|
                \_ ->
                    case extractGears testGearSpecs [ gearRef "3647.dat" 0 0 0 ] emptyCache of
                        [ inst ] ->
                            Expect.equal "3647.dat" inst.spec.partFile

                        _ ->
                            Expect.fail "Expected exactly one gear"
            , test "world position reflects translation" <|
                \_ ->
                    case extractGears testGearSpecs [ gearRef "3647.dat" 10 0 0 ] emptyCache of
                        [ inst ] ->
                            Math.Vector3.getX inst.worldPosition
                                |> Expect.within (Expect.Absolute 1.0e-4) 10.0

                        _ ->
                            Expect.fail "Expected exactly one gear"
            , test "two gear refs produce two instances" <|
                \_ ->
                    let
                        lines =
                            [ gearRef "3647.dat" 0 0 0
                            , gearRef "4019.dat" 40 0 0
                            ]
                    in
                    extractGears testGearSpecs lines emptyCache
                        |> List.length
                        |> Expect.equal 2
            , test "IDs are assigned 0, 1, 2, ..." <|
                \_ ->
                    let
                        lines =
                            [ gearRef "3647.dat" 0 0 0
                            , gearRef "4019.dat" 40 0 0
                            ]

                        ids =
                            extractGears testGearSpecs lines emptyCache |> List.map .id
                    in
                    Expect.equal [ 0, 1 ] ids
            ]
        , describe "buildGearGraph — adjacency"
            [ test "no instances → empty connections" <|
                \_ ->
                    buildGearGraph []
                        |> .connections
                        |> Dict.isEmpty
                        |> Expect.equal True
            , test "single gear → no connections" <|
                \_ ->
                    let
                        gear =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }
                    in
                    buildGearGraph [ gear ]
                        |> .connections
                        |> Dict.isEmpty
                        |> Expect.equal True
            , test "8T + 16T at exact mesh distance → connected" <|
                \_ ->
                    -- pitch radii: 10 + 20 = 30 LDU apart on X axis
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 30 0 0, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> List.member 1
                        |> Expect.equal True
            , test "connection is symmetric (both directions)" <|
                \_ ->
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 30 0 0, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Expect.all
                        [ \g -> Dict.get 0 g.connections |> Maybe.withDefault [] |> List.member 1 |> Expect.equal True
                        , \g -> Dict.get 1 g.connections |> Maybe.withDefault [] |> List.member 0 |> Expect.equal True
                        ]
                        graph
            , test "two gears too far apart → not connected" <|
                \_ ->
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 200 0 0, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> Expect.equal []
            , test "parallel gears with axial offset do not mesh even at same distance" <|
                \_ ->
                    let
                        -- Local Z is gear axis for identity transform.
                        -- g2 is offset in both X and Z, but keeps centre distance ~30.
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1
                            , spec = spec16T
                            , color = 16
                            , worldPosition = vec3 29.5804 0 5
                            , worldMatrix = Mat4.identity
                            }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> Expect.equal []
            , test "instances array length matches input" <|
                \_ ->
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 40 0 0, worldMatrix = Mat4.identity }
                    in
                    buildGearGraph [ g1, g2 ]
                        |> .instances
                        |> Array.length
                        |> Expect.equal 2
            , test "bevel gears connect when axes are perpendicular" <|
                \_ ->
                    let
                        g1 =
                            { id = 0, spec = specBevel20T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1
                            , spec = specBevel20T
                            , color = 16
                            , worldPosition = vec3 50 0 0
                            , worldMatrix = Mat4.makeRotate (pi / 2) (vec3 1 0 0)
                            }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> List.member 1
                        |> Expect.equal True
            , test "bevel gears do not connect when axes are parallel" <|
                \_ ->
                    let
                        g1 =
                            { id = 0, spec = specBevel20T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1
                            , spec = specBevel20T
                            , color = 16
                            , worldPosition = vec3 50 0 0
                            , worldMatrix = Mat4.identity
                            }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> Expect.equal []
            , test "crown meshes with spur when axes are perpendicular" <|
                \_ ->
                    let
                        crown =
                            { id = 0, spec = specCrown24T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        spur =
                            { id = 1
                            , spec = spec8T
                            , color = 16
                            , worldPosition = vec3 40 0 0
                            , worldMatrix = Mat4.makeRotate (pi / 2) (vec3 1 0 0)
                            }

                        graph =
                            buildGearGraph [ crown, spur ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> List.member 1
                        |> Expect.equal True
            , test "crown does not mesh with spur when axes are parallel" <|
                \_ ->
                    let
                        crown =
                            { id = 0, spec = specCrown24T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        spur =
                            { id = 1
                            , spec = spec8T
                            , color = 16
                            , worldPosition = vec3 40 0 0
                            , worldMatrix = Mat4.identity
                            }

                        graph =
                            buildGearGraph [ crown, spur ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> Expect.equal []
            ]
        ]
