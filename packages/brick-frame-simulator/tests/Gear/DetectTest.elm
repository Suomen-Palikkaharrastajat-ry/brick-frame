module Gear.DetectTest exposing (suite)

{-| Unit tests for gear detection and graph construction logic.
-}

import Array
import Dict
import Expect
import Gear.Detect exposing (buildGearGraph, buildGearGraphReference, extractGears)
import Gear.Types exposing (GearInstance, GearSpec)
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


specWorm : GearSpec
specWorm =
    { partFile = "4716.dat", teeth = 1, pitchRadius = 12.0 }


spec12T : GearSpec
spec12T =
    { partFile = "69778.dat", teeth = 12, pitchRadius = 12.0 }


testGearSpecs : List GearSpec
testGearSpecs =
    [ spec8T
    , spec16T
    , specBevel20T
    , specCrown24T
    ]


{-| Build a gear instance with the given id, spec, world matrix and position.
-}
gearInst : Int -> GearSpec -> Mat4.Mat4 -> Math.Vector3.Vec3 -> GearInstance
gearInst id spec mat pos =
    { id = id, spec = spec, color = 16, worldPosition = pos, worldMatrix = mat }


{-| Assert the spatial-hash `buildGearGraph` produces byte-identical output to
the O(n²) `buildGearGraphReference` — same connections and rigid axles, in the
same adjacency-list order.
-}
expectSameGraph : List GearInstance -> Expect.Expectation
expectSameGraph insts =
    let
        fast =
            buildGearGraph insts

        ref =
            buildGearGraphReference insts
    in
    Expect.all
        [ \_ -> Expect.equal ref.connections fast.connections
        , \_ -> Expect.equal ref.rigidAxles fast.rigidAxles
        ]
        ()



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
            , test "worm drives 12T when axes are perpendicular and centres are 24 LDU apart" <|
                \_ ->
                    -- Mirrors wheeler-plain.ldr: worm axis along X (identity Z→X via 90° Y rotation),
                    -- 12T axis along Z (identity). Centre distance = 24 LDU along Y.
                    let
                        worm =
                            { id = 0
                            , spec = specWorm
                            , color = 16
                            , worldPosition = vec3 0 0 0
                            , worldMatrix = Mat4.makeRotate (pi / 2) (vec3 0 1 0)
                            }

                        gear12T =
                            { id = 1
                            , spec = spec12T
                            , color = 16
                            , worldPosition = vec3 0 24 0
                            , worldMatrix = Mat4.identity
                            }

                        graph =
                            buildGearGraph [ worm, gear12T ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> List.member 1
                        |> Expect.equal True
            , test "worm→12T connection is one-way (self-locking)" <|
                \_ ->
                    let
                        worm =
                            { id = 0
                            , spec = specWorm
                            , color = 16
                            , worldPosition = vec3 0 0 0
                            , worldMatrix = Mat4.makeRotate (pi / 2) (vec3 0 1 0)
                            }

                        gear12T =
                            { id = 1
                            , spec = spec12T
                            , color = 16
                            , worldPosition = vec3 0 24 0
                            , worldMatrix = Mat4.identity
                            }

                        graph =
                            buildGearGraph [ worm, gear12T ]
                    in
                    -- worm → 12T exists, but 12T → worm must not
                    Expect.all
                        [ \g -> Dict.get 0 g.connections |> Maybe.withDefault [] |> List.member 1 |> Expect.equal True
                        , \g -> Dict.get 1 g.connections |> Maybe.withDefault [] |> Expect.equal []
                        ]
                        graph
            ]
        , describe "buildGearGraph — rigidAxles"
            [ test "two gears on the same Z-axis → co-axial coupling" <|
                \_ ->
                    -- Both gears use identity transform → axis is Z.
                    -- They are separated along Z only, so radial offset = 0.
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 0 0 40, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Expect.all
                        [ \g -> Dict.get 0 g.rigidAxles |> Maybe.withDefault [] |> List.member 1 |> Expect.equal True
                        , \g -> Dict.get 1 g.rigidAxles |> Maybe.withDefault [] |> List.member 0 |> Expect.equal True
                        ]
                        graph
            , test "co-axial gears do NOT appear in meshing connections" <|
                \_ ->
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 0 0 40, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> Expect.equal []
            , test "two gears with parallel but offset axes → not co-axial" <|
                \_ ->
                    -- Shifted 5 LDU radially — axes are parallel but not the same shaft.
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 5 0 40, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.rigidAxles
                        |> Maybe.withDefault []
                        |> Expect.equal []
            ]
        , describe "buildGearGraph — equivalence with reference"
            [ test "empty model" <|
                \_ -> expectSameGraph []
            , test "meshing chain along X" <|
                \_ ->
                    -- 8T/16T/8T at successive pitch-sum spacings; all identity
                    -- matrices (axis Z), so parallel spur gears mesh.
                    expectSameGraph
                        [ gearInst 0 spec8T Mat4.identity (vec3 0 0 0)
                        , gearInst 1 spec16T Mat4.identity (vec3 30 0 0)
                        , gearInst 2 spec8T Mat4.identity (vec3 60 0 0)
                        ]
            , test "long co-axial stack along Z" <|
                \_ ->
                    -- Gears far apart along a shared Z axle: co-axial at any
                    -- separation. A position-only hash would drop these links.
                    expectSameGraph
                        [ gearInst 0 spec8T Mat4.identity (vec3 0 0 0)
                        , gearInst 1 spec16T Mat4.identity (vec3 0 0 200)
                        , gearInst 2 spec8T Mat4.identity (vec3 0 0 500)
                        ]
            , test "mixed meshing, co-axial and unrelated gears" <|
                \_ ->
                    expectSameGraph
                        [ gearInst 0 spec8T Mat4.identity (vec3 0 0 0)
                        , gearInst 1 spec16T Mat4.identity (vec3 30 0 0)
                        , gearInst 2 spec8T Mat4.identity (vec3 0 0 300)
                        , gearInst 3 spec16T Mat4.identity (vec3 300 0 0)
                        , gearInst 4 spec8T Mat4.identity (vec3 30 0 300)
                        ]
            , test "worm perpendicular to wheel (one-way edge)" <|
                \_ ->
                    expectSameGraph
                        [ gearInst 0 specWorm (Mat4.makeRotate (pi / 2) (vec3 0 1 0)) (vec3 0 0 0)
                        , gearInst 1 spec12T Mat4.identity (vec3 0 24 0)
                        ]
            , test "meshing pair straddling a spatial-hash cell boundary" <|
                \_ ->
                    -- maxPitchRadius = 20 → meshCell = 42. Placing the pair at
                    -- x = 41 and x = 71 puts them in cells 0 and 1 respectively.
                    expectSameGraph
                        [ gearInst 0 spec8T Mat4.identity (vec3 41 0 0)
                        , gearInst 1 spec16T Mat4.identity (vec3 71 0 0)
                        ]
            , test "long co-axial pair is present in rigidAxles" <|
                \_ ->
                    -- The decisive regression: a naive position hash drops this.
                    let
                        graph =
                            buildGearGraph
                                [ gearInst 0 spec8T Mat4.identity (vec3 0 0 0)
                                , gearInst 1 spec16T Mat4.identity (vec3 0 0 500)
                                ]
                    in
                    Dict.get 0 graph.rigidAxles
                        |> Maybe.withDefault []
                        |> List.member 1
                        |> Expect.equal True
            ]
        ]
