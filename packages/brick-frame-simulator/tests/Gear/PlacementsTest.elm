module Gear.PlacementsTest exposing (suite)

{-| Tests for matching detected gears, axles and driven parts to their
top-level placements in a document.
-}

import Dict
import Expect
import Gear.Placements as Placements
import LDraw.Document as Document
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import Test exposing (Test, describe, test)


{-| Two placements: a gear at the origin and an axle at (40, -60, 0).
-}
doc : Document.Document
doc =
    Document.parse "1 7 0 0 0 1 0 0 0 1 0 0 0 1 3648.dat\n1 0 40 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat"


suite : Test
suite =
    describe "Gear.Placements"
        [ describe "placementAt"
            [ test "finds the placement with the same file at the same origin" <|
                \_ ->
                    Placements.placementAt doc "3705.dat" (vec3 40 -60 0)
                        |> Expect.equal (Just 1)
            , test "the file must match" <|
                \_ ->
                    Placements.placementAt doc "3648.dat" (vec3 40 -60 0)
                        |> Expect.equal Nothing
            , test "an origin 0.5 LDU away does not match" <|
                \_ ->
                    Placements.placementAt doc "3705.dat" (vec3 40.5 -60 0)
                        |> Expect.equal Nothing
            , test "float noise well under 0.01 LDU still matches" <|
                \_ ->
                    Placements.placementAt doc "3705.dat" (vec3 40.001 -60 -0.001)
                        |> Expect.equal (Just 1)
            ]
        , describe "partSources"
            [ test "a driven axle is paired with its gear and its placement" <|
                \_ ->
                    Placements.partSources doc
                        [ { file = "3705.dat", transform = Mat4.makeTranslate3 40 -60 0, drivingGearId = 0 } ]
                        |> Expect.equal [ ( 0, 1 ) ]
            , test "a part with no placement of its own is left out" <|
                \_ ->
                    Placements.partSources doc
                        [ { file = "3705.dat", transform = Mat4.makeTranslate3 300 -60 0, drivingGearId = 0 } ]
                        |> Expect.equal []
            , test "the origin is taken through the part's transform" <|
                \_ ->
                    Placements.partSources doc
                        [ { file = "3705.dat"
                          , transform = Mat4.mul (Mat4.makeTranslate3 40 -60 0) (Mat4.makeRotate (pi / 2) (vec3 0 1 0))
                          , drivingGearId = 3
                          }
                        ]
                        |> Expect.equal [ ( 3, 1 ) ]
            ]
        , describe "glued placements"
            [ test "a glued part is excluded from partSources" <|
                \_ ->
                    let
                        gluedDoc =
                            Document.setGlued True 1 doc
                    in
                    Placements.partSources gluedDoc
                        [ { file = "3705.dat", transform = Mat4.makeTranslate3 40 -60 0, drivingGearId = 0 } ]
                        |> Expect.equal []
            , test "a glued gear is excluded from gearSources" <|
                \_ ->
                    let
                        gluedDoc =
                            Document.setGlued True 0 doc
                    in
                    Placements.gearSources gluedDoc
                        [ { id = 0, spec = { partFile = "3648.dat", teeth = 24, pitchRadius = 30 }, color = 7, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity } ]
                        |> Expect.equal Dict.empty
            ]
        ]
