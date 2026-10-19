module HintsTest exposing (suite)

{-| Tests for the live gear hints.
-}

import Expect
import Gear.Types exposing (GearSpec)
import LDraw.Document as Document
import Set
import Studio.Hints as Hints
import Test exposing (Test, describe, test)


specs : List GearSpec
specs =
    [ { partFile = "3647.dat", teeth = 8, pitchRadius = 8 }
    , { partFile = "4019.dat", teeth = 16, pitchRadius = 16 }
    ]


doc : String -> Document.Document
doc =
    Document.parse


suite : Test
suite =
    describe "Studio.Hints"
        [ test "a model without gears has no hints" <|
            \_ ->
                Hints.analyse specs (doc "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n")
                    |> Expect.equal Hints.empty
        , test "two gears at the right distance mesh" <|
            \_ ->
                Hints.analyse specs (doc "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n1 4 24 0 0 1 0 0 0 1 0 0 0 1 4019.dat\n")
                    |> Expect.all
                        [ \h -> Expect.equal 2 h.gears
                        , \h -> Expect.equal (Set.fromList [ 0, 1 ]) h.meshed
                        , \h -> Expect.equal Set.empty h.isolated
                        ]
        , test "a gear with nothing to mesh with can't turn" <|
            \_ ->
                Hints.analyse specs (doc "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n1 4 24 0 0 1 0 0 0 1 0 0 0 1 4019.dat\n1 4 200 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n")
                    |> Expect.all
                        [ \h -> Expect.equal 3 h.gears
                        , \h -> Expect.equal (Set.fromList [ 2 ]) h.isolated
                        ]
        , test "a lone gear can't turn" <|
            \_ ->
                Hints.analyse specs (doc "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n")
                    |> .isolated
                    |> Expect.equal (Set.fromList [ 0 ])
        , test "other parts in between do not matter" <|
            \_ ->
                Hints.analyse specs (doc "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n1 4 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n1 4 50 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n1 4 24 0 0 1 0 0 0 1 0 0 0 1 4019.dat\n")
                    |> .meshed
                    |> Expect.equal (Set.fromList [ 1, 3 ])
        , describe "summary"
            [ test "says nothing without gears" <|
                \_ -> Expect.equal "" (Hints.summary Hints.empty)
            , test "reports a working train" <|
                \_ ->
                    Hints.summary { gears = 2, meshed = Set.fromList [ 0, 1 ], isolated = Set.empty }
                        |> Expect.equal "⚙ 2 gears, all meshed"
            , test "warns about gears that can't turn" <|
                \_ ->
                    Hints.summary { gears = 3, meshed = Set.fromList [ 0, 1 ], isolated = Set.fromList [ 2 ] }
                        |> Expect.equal "⚙ 3 gears, 1 can't turn"
            , test "uses the singular for one" <|
                \_ ->
                    Hints.summary { gears = 1, meshed = Set.empty, isolated = Set.fromList [ 0 ] }
                        |> Expect.equal "⚙ 1 gear, 1 can't turn"
            ]
        ]
