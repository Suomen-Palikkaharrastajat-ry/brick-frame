module Gear.ComponentsTest exposing (suite)

{-| Unit tests for component extraction from LDraw model hierarchies.
-}

import Dict
import Expect
import Gear.Components exposing (ComponentKind(..), ComponentSpec, extractComponents)
import LDraw.Resolve exposing (PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Test exposing (Test, describe, test)


componentSpecs : List ComponentSpec
componentSpecs =
    [ { partFile = "3706.dat", kind = AxleLike }
    , { partFile = "32316.dat", kind = Beam }
    ]


suite : Test
suite =
    describe "Gear.Components.extractComponents"
        [ test "detects direct top-level axle and beam refs" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef { color = 16, transform = Mat4.identity, file = "3706.dat" }
                        , SubFileRef { color = 16, transform = Mat4.identity, file = "32316.dat" }
                        ]

                    instances =
                        extractComponents componentSpecs lines Dict.empty
                in
                Expect.equal 2 (List.length instances)
        , test "recurses into cached subfiles" <|
            \_ ->
                let
                    topLevel =
                        [ SubFileRef { color = 16, transform = Mat4.identity, file = "assembly.dat" } ]

                    cache =
                        Dict.fromList
                            [ ( "assembly.dat"
                              , Loaded
                                    [ SubFileRef
                                        { color = 16
                                        , transform = Mat4.makeTranslate3 10 0 0
                                        , file = "3706.dat"
                                        }
                                    ]
                              )
                            ]

                    instances =
                        extractComponents componentSpecs topLevel cache
                in
                Expect.equal 1 (List.length instances)
        , test "world position includes transform" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.makeTranslate3 12 0 0
                            , file = "3706.dat"
                            }
                        ]
                in
                case extractComponents componentSpecs lines Dict.empty of
                    [ inst ] ->
                        inst.worldPosition
                            |> Vec3.getX
                            |> Expect.within (Expect.Absolute 1.0e-4) 12.0

                    _ ->
                        Expect.fail "Expected exactly one component"
        ]
