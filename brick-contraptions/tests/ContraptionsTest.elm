module ContraptionsTest exposing (suite)

{-| Unit tests for Brick Contraptions and machine accomplishment logic.
-}

import Contraption.Catalog as Catalog exposing (Category(..))
import Contraption.Machine as Machine exposing (MachineState)
import Dict
import EmbeddedParts
import Expect
import LDraw.Document as Document
import Main exposing (Flags, Mode(..), Model, Msg(..), init, update)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Studio.Edit as Edit exposing (NewPart)
import Test exposing (Test, describe, test)
import Url


suite : Test
suite =
    describe "Brick Contraptions"
        [ describe "Curated Catalog"
            [ test "curatedParts contains all required gears" <|
                \_ ->
                    let
                        gearFiles =
                            Catalog.partsForCategory GearsCategory
                                |> List.map .file
                    in
                    Expect.all
                        [ \files -> Expect.equal (List.member "3647.dat" files) True
                        , \files -> Expect.equal (List.member "4019.dat" files) True
                        , \files -> Expect.equal (List.member "3648.dat" files) True
                        , \files -> Expect.equal (List.member "3649.dat" files) True
                        ]
                        gearFiles
            , test "curatedParts contains beams, axles, and pins" <|
                \_ ->
                    let
                        beamFiles =
                            Catalog.partsForCategory BeamsCategory |> List.map .file

                        axleFiles =
                            Catalog.partsForCategory AxlesCategory |> List.map .file

                        pinFiles =
                            Catalog.partsForCategory PinsCategory |> List.map .file
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.length beamFiles >= 3) True
                        , \_ -> Expect.equal (List.length axleFiles >= 4) True
                        , \_ -> Expect.equal (List.length pinFiles >= 2) True
                        ]
                        ()
            ]
        , describe "Machine Accomplishment Analysis"
            [ test "empty document is not accomplished" <|
                \_ ->
                    let
                        state =
                            Machine.analyseMachine Document.empty EmbeddedParts.cache
                    in
                    Expect.equal (Machine.isAccomplished state) False
            , test "single gear alone is not accomplished" <|
                \_ ->
                    let
                        part1 =
                            { part = "3647.dat", color = 7, matrix = Mat4.makeTranslate (vec3 0 0 0) }

                        ( doc, _ ) =
                            Edit.apply (Edit.Add [ part1 ]) Document.empty

                        state =
                            Machine.analyseMachine doc EmbeddedParts.cache
                    in
                    Expect.all
                        [ \s -> Expect.equal (List.length s.gears) 1
                        , \s -> Expect.equal (Machine.isAccomplished s) False
                        ]
                        state
            , test "two meshing gears (8T and 16T at distance 30) form an accomplished machine" <|
                \_ ->
                    let
                        -- 8T pitch radius = 10, 16T pitch radius = 20. Total center distance = 30
                        part1 =
                            { part = "3647.dat", color = 7, matrix = Mat4.makeTranslate (vec3 0 0 0) }

                        part2 =
                            { part = "4019.dat", color = 7, matrix = Mat4.makeTranslate (vec3 30 0 0) }

                        ( doc, _ ) =
                            Edit.apply (Edit.Add [ part1, part2 ]) Document.empty

                        state =
                            Machine.analyseMachine doc EmbeddedParts.cache
                    in
                    Expect.all
                        [ \s -> Expect.equal (List.length s.gears) 2
                        , \s -> Expect.equal s.rotatingCount 2
                        , \s -> Expect.equal s.isSingleMachine True
                        , \s -> Expect.equal (Machine.isAccomplished s) True
                        ]
                        state
            , test "two distant gears that do not mesh are not accomplished" <|
                \_ ->
                    let
                        part1 =
                            { part = "3647.dat", color = 7, matrix = Mat4.makeTranslate (vec3 0 0 0) }

                        part2 =
                            { part = "4019.dat", color = 7, matrix = Mat4.makeTranslate (vec3 150 0 0) }

                        ( doc, _ ) =
                            Edit.apply (Edit.Add [ part1, part2 ]) Document.empty

                        state =
                            Machine.analyseMachine doc EmbeddedParts.cache
                    in
                    Expect.all
                        [ \s -> Expect.equal (List.length s.gears) 2
                        , \s -> Expect.equal s.rotatingCount 1
                        , \s -> Expect.equal s.isSingleMachine False
                        , \s -> Expect.equal (Machine.isAccomplished s) False
                        ]
                        state
            ]
        , describe "Application Interaction"
            [ test "starts in EditMode and toggles to SimulateMode" <|
                \_ ->
                    let
                        ( model0, _ ) =
                            init testFlags

                        ( model1, _ ) =
                            update ToggleMode model0

                        ( model2, _ ) =
                            update ToggleMode model1
                    in
                    Expect.all
                        [ \_ -> Expect.equal model0.mode EditMode
                        , \_ -> Expect.equal model1.mode SimulateMode
                        , \_ -> Expect.equal model2.mode EditMode
                        ]
                        ()
            , test "SetCategory updates active category tab" <|
                \_ ->
                    let
                        ( model0, _ ) =
                            init testFlags

                        ( model1, _ ) =
                            update (SetCategory BeamsCategory) model0
                    in
                    Expect.equal model1.activeCategory BeamsCategory
            ]
        , describe "URL Payload Initialization"
            [ test "initializes document and analyzes machine immediately from hash modelText" <|
                \_ ->
                    let
                        -- Model with two meshing gears at distance 30 (accomplished machine)
                        modelStr =
                            "1 7 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n1 7 30 0 0 1 0 0 0 1 0 0 0 1 4019.dat"

                        flags =
                            { testFlags | initialHash = "#model=" ++ Url.percentEncode modelStr }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 2 (List.length m.machine.gears)
                        , \m -> Expect.equal 2 m.machine.rotatingCount
                        , \m -> Expect.equal True (Machine.isAccomplished m.machine)
                        ]
                        model
            , test "populates project parts category from hash parts" <|
                \_ ->
                    let
                        flags =
                            { testFlags | initialHash = "#parts=3647.dat:7:3,32316.dat:1:2" }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 2 (List.length m.projectParts)
                        , \m -> Expect.equal ProjectCategory m.activeCategory
                        , \m -> Expect.equal 2 (List.length (Catalog.partsForCategoryWithProject m.projectParts ProjectCategory))
                        ]
                        model
            ]
        ]


testFlags : Flags
testFlags =
    { ldrawBase = "/ldraw"
    , ldrawFallbackBase = ""
    , initialHash = ""
    , supportsPointerEvents = True
    }
