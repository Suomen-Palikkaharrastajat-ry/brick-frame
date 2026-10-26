module TechnicTest exposing (suite)

{-| Unit tests for Technic mode in Brick Play: how it is derived, what it switches
on, the Technic catalog dock, Technic placement and the machine's accomplishment.
-}

import Contraption.Animation as Animation
import Contraption.Catalog as Catalog exposing (Category(..))
import Contraption.Machine as Machine exposing (MachineState)
import Contraption.Placement as Placement
import Data
import Dict
import EmbeddedParts
import Expect
import Gear.Detect as Detect
import Gear.Phase as Phase
import Gear.Placements as Placements
import Html.Attributes as Attr
import LDraw.Document as Document
import LDraw.Resolve exposing (PartStatus(..))
import Main exposing (Flags, Model, Msg(..), accomplishmentBanner, init, shareHash, technicMode, thumbnailRequests, update, view, viewToast)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Render.Camera as Camera
import Set
import Studio.Core as Core
import Studio.DragPlane as DragPlane
import Studio.Edit as Edit exposing (NewPart)
import Studio.GearFeedback as GearFeedback
import Studio.Shortcuts exposing (TurnDirection(..))
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time
import UI.SnapshotModal as SnapshotModal
import Url
import Viewport


suite : Test
suite =
    describe "Brick Play Technic"
        [ menuTests
        , snapshotTests
        , sharingAndToastTests
        , fineTests
        , describe "Curated Catalog"
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
            [ test "starts paused and TogglePlay plays and pauses" <|
                \_ ->
                    let
                        ( model0, _ ) =
                            init (modelFlags pair8x24)

                        ( model1, _ ) =
                            update TogglePlay model0

                        ( model2, _ ) =
                            update TogglePlay model1
                    in
                    Expect.all
                        [ \_ -> Expect.equal model0.playing False
                        , \_ -> Expect.equal model1.playing True
                        , \_ -> Expect.equal model2.playing False
                        ]
                        ()
            , test "Space plays and pauses while editing" <|
                \_ ->
                    let
                        ( model0, _ ) =
                            init (modelFlags pair8x24)

                        space =
                            { key = " "
                            , ctrl = False
                            , meta = False
                            , alt = False
                            , shift = False
                            , repeat = False
                            , targetTag = "BODY"
                            , targetEditable = False
                            , targetRole = ""
                            }

                        ( model1, _ ) =
                            update (StudioMsg (Core.ViewportMsg (Viewport.KeyPressed space))) model0
                    in
                    Expect.equal True model1.playing
            , test "the help dialog toggles and pauses the studio's editing keys" <|
                \_ ->
                    let
                        ( model0, _ ) =
                            init testFlags

                        ( model1, _ ) =
                            update ToggleHelp model0

                        ( model2, _ ) =
                            update ToggleHelp model1
                    in
                    Expect.all
                        [ \_ -> Expect.equal ( False, False ) ( model0.helpOpen, model0.studio.helpVisible )
                        , \_ -> Expect.equal ( True, True ) ( model1.helpOpen, model1.studio.helpVisible )
                        , \_ -> Expect.equal ( False, False ) ( model2.helpOpen, model2.studio.helpVisible )
                        ]
                        ()
            , test "? opens the help dialog and Esc closes it" <|
                \_ ->
                    let
                        ( model0, _ ) =
                            init testFlags

                        ( model1, _ ) =
                            update (pressKey { key = "?", ctrl = False, shift = True }) model0

                        ( model2, _ ) =
                            update (pressKey { key = "Escape", ctrl = False, shift = False }) model1
                    in
                    Expect.equal [ False, True, False ]
                        [ model0.helpOpen, model1.helpOpen, model2.helpOpen ]
            , test "W moves the selected part, and leaves the camera alone" <|
                \_ ->
                    let
                        ( model0, _ ) =
                            init testFlags

                        ( studioWithGear, _, _ ) =
                            Core.addPart "3647.dat" 7 (vec3 0 0 0) model0.studio

                        model1 =
                            { model0 | studio = studioWithGear }

                        ( model2, _ ) =
                            update (pressKey { key = "a", ctrl = True, shift = False }) model1

                        ( model3, _ ) =
                            update (pressKey { key = "w", ctrl = False, shift = False }) model2

                        -- Run the animation frames, so a camera glide (the pivot
                        -- following the moved selection) or a held W walking the
                        -- camera would both have shown by now.
                        settle model =
                            List.foldl
                                (\ms m -> Tuple.first (update (StudioMsg (Core.ViewportMsg (Viewport.AnimationFrame (Time.millisToPosix ms)))) m))
                                model
                                [ 0, 500, 1000 ]

                        eye model =
                            Camera.position (settle model).studio.viewport.camera
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (Core.hasSelection model3.studio)
                        , \_ -> Expect.notEqual model2.studio.document model3.studio.document
                        , \_ -> Expect.within (Expect.Absolute 1.0e-6) 0 (Vec3.distance (eye model2) (eye model3))
                        ]
                        ()
            , test "Esc clears the selection" <|
                \_ ->
                    let
                        ( model0, _ ) =
                            init testFlags

                        ( studioWithGear, _, _ ) =
                            Core.addPart "3647.dat" 7 (vec3 0 0 0) model0.studio

                        model1 =
                            { model0 | studio = studioWithGear }

                        ( model2, _ ) =
                            update (pressKey { key = "a", ctrl = True, shift = False }) model1

                        ( model3, _ ) =
                            update (pressKey { key = "Escape", ctrl = False, shift = False }) model2
                    in
                    Expect.equal [ True, False ]
                        [ Core.hasSelection model2.studio, Core.hasSelection model3.studio ]
            , test "the studio drags parts, or pans on empty canvas, without a gizmo" <|
                \_ ->
                    let
                        ( model, _ ) =
                            init testFlags
                    in
                    Expect.all
                        [ \m -> Expect.equal (Core.DragPartsOrPan Core.OneFingerPans) m.studio.dragMode
                        , \m -> Expect.equal Core.NoGizmo m.studio.gizmoStyle
                        ]
                        model
            , test "window size reaches the viewport, so picks line up with the canvas" <|
                \_ ->
                    let
                        ( model, _ ) =
                            init testFlags
                                |> Tuple.first
                                |> update (WindowResized 705 813)
                    in
                    Expect.equal ( 705, 813 ) ( model.studio.viewport.width, model.studio.viewport.height )
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
            , test "populates the counted kit from hash parts, and a gear in it turns Technic mode on" <|
                \_ ->
                    let
                        flags =
                            { testFlags | initialHash = "#parts=3647.dat:7:3,32316.dat:1:2" }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 2 (List.length m.inventory)
                        , \m -> Expect.equal True (technicMode m)
                        , \m -> Expect.equal True m.kitFromUrl
                        ]
                        model
            , test "init with twin 24T example loads beam, 2 axles, 1 gear and project kit" <|
                \_ ->
                    let
                        twin24Model =
                            "1 1 0 -60 -20 0 0 -1 0 1 0 1 0 0 32524.dat\n1 0 0 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat\n1 0 60 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat\n1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3648.dat\n1 7 0 -60 12 1 0 0 0 1 0 0 0 1 6590.dat"

                        twin24Parts =
                            "3648.dat:7:1,6590.dat:7:2"

                        flags =
                            { testFlags | initialHash = "#model=" ++ Url.percentEncode twin24Model ++ "&parts=" ++ Url.percentEncode twin24Parts }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 1 (List.length m.machine.gears)
                        , \m -> Expect.equal 2 (List.length m.inventory)
                        , \m -> Expect.equal True (technicMode m)
                        , \m -> Expect.equal False (Machine.isAccomplished m.machine)
                        ]
                        model
            , test "init with 5:1 reducer example loads beam, 2 axles, 8T gear and 40T kit" <|
                \_ ->
                    let
                        reducerModel =
                            "1 1 0 -60 -20 0 0 -1 0 1 0 1 0 0 32524.dat\n1 0 0 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat\n1 0 60 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat\n1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3647.dat\n1 7 0 -60 12 1 0 0 0 1 0 0 0 1 6590.dat"

                        reducerParts =
                            "3649.dat:7:1,6590.dat:7:2"

                        flags =
                            { testFlags | initialHash = "#model=" ++ Url.percentEncode reducerModel ++ "&parts=" ++ Url.percentEncode reducerParts }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 1 (List.length m.machine.gears)
                        , \m -> Expect.equal 2 (List.length m.inventory)
                        , \m -> Expect.equal True (technicMode m)
                        , \m -> Expect.equal False (Machine.isAccomplished m.machine)
                        ]
                        model
            , test "init with 3-gear train example loads 2 meshing gears and completes with third" <|
                \_ ->
                    let
                        trainModel =
                            "1 1 0 -60 -20 0 0 -1 0 1 0 1 0 0 32524.dat\n1 0 -40 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat\n1 0 0 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat\n1 0 60 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat\n1 7 -40 -60 0 1 0 0 0 1 0 0 0 1 3647.dat\n1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3648.dat"

                        trainParts =
                            "3648.dat:7:1,4019.dat:7:1,6590.dat:7:3"

                        flags =
                            { testFlags | initialHash = "#model=" ++ Url.percentEncode trainModel ++ "&parts=" ++ Url.percentEncode trainParts }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 2 (List.length m.machine.gears)
                        , \m -> Expect.equal 3 (List.length m.inventory)
                        , \m -> Expect.equal True (technicMode m)
                        , \m -> Expect.equal True (Machine.isAccomplished m.machine)
                        ]
                        model
            ]
        , describe "Gear animation"
            [ test "an 8T driving a 24T: opposite ways, the 24T three times slower" <|
                \_ ->
                    let
                        played =
                            init (modelFlags pair8x24)
                                |> Tuple.first
                                |> update TogglePlay
                                |> Tuple.first
                                |> update (Tick 1000)
                                |> Tuple.first

                        angle gid =
                            Dict.get gid played.simulation.gearAngles |> Maybe.withDefault 0
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-6) 1 played.currentTime

                        -- 30 RPM = pi rad/s at the motor (the first gear, the 8T).
                        , \_ -> Expect.within (Expect.Absolute 1.0e-6) pi (angle 0)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-6) (-pi / 3) (angle 1)
                        ]
                        ()
            , test "angles are a pure function of the clock" <|
                \_ ->
                    let
                        playFor ticks =
                            List.foldl (\ms m -> Tuple.first (update (Tick ms) m))
                                (init (modelFlags pair8x24) |> Tuple.first |> update TogglePlay |> Tuple.first)
                                ticks
                    in
                    Expect.equal (playFor [ 1000 ]).simulation.gearAngles (playFor [ 250, 250, 500 ]).simulation.gearAngles
            , test "pausing freezes the clock, and Play resumes from it" <|
                \_ ->
                    let
                        paused =
                            init (modelFlags pair8x24)
                                |> Tuple.first
                                |> update TogglePlay
                                |> Tuple.first
                                |> update (Tick 500)
                                |> Tuple.first
                                |> update TogglePlay
                                |> Tuple.first

                        later =
                            update (Tick 500) paused |> Tuple.first

                        resumed =
                            update TogglePlay later |> Tuple.first |> update (Tick 500) |> Tuple.first
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-6) 0.5 later.currentTime
                        , \_ -> Expect.equal True (Animation.isAnimating { playing = later.playing, currentTime = later.currentTime })
                        , \_ -> Expect.within (Expect.Absolute 1.0e-6) 1 resumed.currentTime
                        ]
                        ()
            , test "an edit after playing rewinds to time 0 and stops" <|
                \_ ->
                    let
                        played =
                            init (modelFlags pair8x24)
                                |> Tuple.first
                                |> update TogglePlay
                                |> Tuple.first
                                |> update (Tick 500)
                                |> Tuple.first

                        edited msg m =
                            update msg m |> Tuple.first

                        paused =
                            edited TogglePlay played

                        rest m =
                            ( m.playing, m.currentTime, Animation.isAnimating { playing = m.playing, currentTime = m.currentTime } )
                    in
                    Expect.all
                        [ \_ -> Expect.equal ( False, 0, False ) (rest (edited (AddCatalogPart gear8) paused))
                        , \_ -> Expect.equal ( False, 0, False ) (rest (edited (AddCatalogPart gear8) played))
                        , \_ -> Expect.equal ( False, 0, False ) (rest (edited (pressKey { key = "a", ctrl = True, shift = False }) paused |> edited (pressKey { key = "d", ctrl = False, shift = False })))
                        , \_ -> Expect.equal ( False, 0, False ) (rest (edited (AddCatalogPart gear8) paused |> edited TogglePlay |> edited (Tick 300) |> edited Undo))
                        ]
                        ()
            , test "selecting while playing does not stop the machine" <|
                \_ ->
                    init (modelFlags pair8x24)
                        |> Tuple.first
                        |> update TogglePlay
                        |> Tuple.first
                        |> update (pressKey { key = "a", ctrl = True, shift = False })
                        |> Tuple.first
                        |> .playing
                        |> Expect.equal True
            , test "each gear maps to its own placement" <|
                \_ ->
                    let
                        doc =
                            Document.parse (pair8x24 ++ "\n1 7 300 0 0 1 0 0 0 1 0 0 0 1 3647.dat")

                        state =
                            Machine.analyseMachine doc EmbeddedParts.cache

                        ids =
                            Document.mainPlacements doc |> List.map .id
                    in
                    Placements.gearSources doc state.gears
                        |> Dict.values
                        |> Expect.equal ids
            , test "only the turning gears are animated and hidden; a lone gear stays static" <|
                \_ ->
                    let
                        doc =
                            Document.parse (pair8x24 ++ "\n1 7 300 0 0 1 0 0 0 1 0 0 0 1 3647.dat")

                        state =
                            Machine.analyseMachine doc EmbeddedParts.cache

                        sources =
                            Placements.gearSources doc state.gears

                        placementIds =
                            Document.mainPlacements doc |> List.map .id

                        animated =
                            Animation.rotating (Set.fromList [ 0, 1 ]) sources []
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Set.fromList [ 0, 1 ]) animated.gears
                        , \_ -> Expect.equal (Set.fromList (List.take 2 placementIds)) animated.sources
                        , \_ -> Expect.equal Animation.none (Animation.rotating Set.empty sources [])
                        ]
                        ()
            , test "a gear without a main-model placement is not animated" <|
                \_ ->
                    Animation.rotating (Set.fromList [ 0, 1 ]) (Dict.fromList [ ( 1, 42 ) ]) []
                        |> Expect.equal { gears = Set.singleton 1, sources = Set.singleton 42 }
            , test "a driven part is hidden with its turning gear; parts of a still gear are not" <|
                \_ ->
                    Animation.rotating (Set.fromList [ 0, 1 ]) (Dict.fromList [ ( 0, 10 ), ( 1, 11 ) ]) [ ( 1, 42 ), ( 7, 50 ) ]
                        |> Expect.equal { gears = Set.fromList [ 0, 1 ], sources = Set.fromList [ 10, 11, 42 ] }
            , test "a driven part whose gear has no main-model placement is not hidden" <|
                \_ ->
                    Animation.rotating (Set.fromList [ 0, 1 ]) (Dict.fromList [ ( 0, 10 ) ]) [ ( 1, 42 ) ]
                        |> Expect.equal { gears = Set.singleton 0, sources = Set.singleton 10 }
            , test "an axle threaded on a turning gear is hidden with it while playing, a free axle is not" <|
                \_ ->
                    let
                        doc =
                            Document.parse (pair8x24 ++ "\n" ++ axleThroughGear ++ "\n" ++ axleAside)

                        ids =
                            Document.mainPlacements doc |> List.map .id

                        axleId =
                            List.drop 2 ids |> List.head

                        asideId =
                            List.drop 3 ids |> List.head

                        playing =
                            init (modelFlags (pair8x24 ++ "\n" ++ axleThroughGear ++ "\n" ++ axleAside))
                                |> Tuple.first
                                |> withAxleMesh
                                |> update TogglePlay
                                |> Tuple.first
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Just True) (Maybe.map (\id -> Set.member id playing.animated.sources) axleId)
                        , \_ -> Expect.equal (Just False) (Maybe.map (\id -> Set.member id playing.animated.sources) asideId)
                        , \_ -> Expect.equal 3 (Set.size playing.animated.sources)
                        ]
                        ()
            , test "an edit while the axle turns stops it and shows the static axle again" <|
                \_ ->
                    let
                        playing =
                            init (modelFlags (pair8x24 ++ "\n" ++ axleThroughGear))
                                |> Tuple.first
                                |> withAxleMesh
                                |> update TogglePlay
                                |> Tuple.first

                        edited =
                            update (AddCatalogPart gear8) playing |> Tuple.first
                    in
                    Expect.all
                        [ \_ -> Expect.equal False (Set.isEmpty playing.animated.sources)
                        , \_ -> Expect.equal True (Set.isEmpty edited.animated.sources)
                        , \_ -> Expect.equal False edited.playing
                        ]
                        ()
            , test "at rest nothing is animated" <|
                \_ ->
                    init (modelFlags pair8x24)
                        |> Tuple.first
                        |> (\m -> Animation.isAnimating { playing = m.playing, currentTime = m.currentTime })
                        |> Expect.equal False
            ]
        , describe "Selection action bar"
            [ test "no axis gizmo: arrows and rings are off from the start" <|
                \_ ->
                    init testFlags
                        |> Tuple.first
                        |> (\m -> Expect.equal Core.NoGizmo m.studio.gizmoStyle)
            , test "the action bar edits the selected part, one undo step each" <|
                \_ ->
                    let
                        ( model0, _ ) =
                            init testFlags

                        ( studioWithGear, _, _ ) =
                            Core.addPart "3647.dat" 7 (vec3 0 0 0) model0.studio

                        added =
                            { model0 | studio = studioWithGear }

                        matrixOf m =
                            Core.getPlacements m.studio |> List.map .matrix

                        run msg =
                            update msg added |> Tuple.first
                    in
                    Expect.all
                        [ \_ -> Expect.notEqual (matrixOf added) (matrixOf (run (TurnSelection TurnLeft)))
                        , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run (TurnSelection TurnRight)))
                        , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run (TurnSelection TipForward)))
                        , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run RaiseSelection))
                        , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run LowerSelection))
                        , \_ -> Expect.equal 0 (List.length (Core.getPlacements (run Delete).studio))
                        , \_ -> Expect.equal (matrixOf added) (matrixOf (run RaiseSelection |> update Undo |> Tuple.first))
                        , \_ -> Expect.equal 0 (List.length (run Delete).machine.gears)
                        ]
                        ()
            ]
        , dockThumbnailTests
        , gearFeedbackTests
        , dockPlacementTests
        , touchTests
        , bannerTests
        , technicTests
        ]


dockThumbnailTests : Test
dockThumbnailTests =
    let
        -- What the dock lists: the catalog of its tab in Technic mode (the URL
        -- kit as the Project tab), else the kit.
        dockPairs model =
            if technicMode model then
                let
                    projectParts =
                        if model.kitFromUrl then
                            List.map Catalog.fromKitItem model.inventory

                        else
                            []
                in
                List.map (\item -> ( item.file, item.color )) (Catalog.partsForCategoryWithProject projectParts model.activeCategory)

            else
                List.map (\item -> ( item.part, item.color )) model.inventory

        requestedFiles model =
            thumbnailRequests (dockPairs model) model.thumbnails
                |> List.map .file

        loaded file color model =
            update (ThumbnailLoaded { file = file, color = color, url = "data:image/png;base64,AA", title = "" }) model
                |> Tuple.first

        technicStart =
            Tuple.first (init (modelFlags pair8x24))
    in
    describe "Dock thumbnails"
        [ test "init asks for the gears of the catalog, in catalog order, in Technic mode" <|
            \_ ->
                Expect.equal [ "3647.dat", "4019.dat", "3648.dat", "3649.dat" ]
                    (requestedFiles technicStart)
        , test "requests carry the colour's rgb from the colour table" <|
            \_ ->
                case thumbnailRequests [ ( "3647.dat", 7 ) ] Dict.empty of
                    [ request ] ->
                        Expect.all
                            [ \r -> Expect.equal 7 r.color
                            , \r -> Expect.equal 3 (List.length r.rgb)
                            , \r -> Expect.equal True (List.all (\c -> c >= 0 && c <= 1) r.rgb)
                            ]
                            request

                    other ->
                        Expect.fail ("expected one request, got " ++ String.fromInt (List.length other))
        , test "a category change asks for that category's parts" <|
            \_ ->
                let
                    model =
                        technicStart
                            |> update (SetCategory BeamsCategory)
                            |> Tuple.first
                in
                Expect.equal (Catalog.partsForCategory BeamsCategory |> List.map .file) (requestedFiles model)
        , test "the kit from #parts= is requested on init, and no catalog is" <|
            \_ ->
                let
                    ( model, _ ) =
                        init { testFlags | initialHash = "#parts=3647.dat:7:2,32316.dat:1:1" }
                in
                Expect.equal [ "3647.dat", "32316.dat" ] (requestedFiles model)
        , test "thumbnailLoaded stores the image under file and colour" <|
            \_ ->
                let
                    model =
                        loaded "3647.dat" 7 (Tuple.first (init testFlags))
                in
                Expect.equal (Just "data:image/png;base64,AA")
                    (Dict.get "3647.dat:7" model.thumbnails |> Maybe.map .url)
        , test "loaded parts are not asked for again" <|
            \_ ->
                let
                    model =
                        technicStart
                            |> loaded "3647.dat" 7
                            |> loaded "3648.dat" 7
                in
                Expect.equal [ "4019.dat", "3649.dat" ] (requestedFiles model)
        , test "a part in another colour is a different thumbnail" <|
            \_ ->
                let
                    model =
                        loaded "3647.dat" 4 technicStart
                in
                Expect.equal True (List.member "3647.dat" (requestedFiles model))
        , test "duplicates in the list are asked for once" <|
            \_ ->
                Expect.equal 1 (List.length (thumbnailRequests [ ( "3647.dat", 7 ), ( "3647.dat", 7 ) ] Dict.empty))
        ]


{-| An 8T at the origin meshing with a 24T 40 LDU away (pitch radii 10 + 30).
-}
pair8x24 : String
pair8x24 =
    "1 7 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n1 7 40 0 0 1 0 0 0 1 0 0 0 1 3648.dat"


{-| An axle along Z through the 24T at X = 40 (its local X turned to Z).
-}
axleThroughGear : String
axleThroughGear =
    "1 0 40 0 0 0 0 -1 0 1 0 1 0 0 3705.dat"


{-| An axle along Z well away from both gears: nothing drives it.
-}
axleAside : String
axleAside =
    "1 0 300 0 0 0 0 -1 0 1 0 1 0 0 3705.dat"


{-| The model with its part cache holding an axle mesh. The embedded cache has
no axles, and a part only turns once its mesh has loaded (as in the browser).
-}
withAxleMesh : Model -> Model
withAxleMesh model =
    let
        studio =
            model.studio

        viewport =
            studio.viewport
    in
    { model
        | studio =
            { studio
                | viewport = { viewport | partCache = Dict.insert "3705.dat" (Loaded []) viewport.partCache }
            }
    }


modelFlags : String -> Flags
modelFlags text =
    { testFlags | initialHash = "#model=" ++ Url.percentEncode text }


gear8 : Catalog.PartItem
gear8 =
    case Catalog.partsForCategory GearsCategory |> List.filter (\item -> item.file == "3647.dat") of
        item :: _ ->
            item

        [] ->
            { name = "8T", file = "3647.dat", color = 7, category = GearsCategory, description = "" }


testFlags : Flags
testFlags =
    { ldrawBase = "/ldraw"
    , ldrawFallbackBase = ""
    , initialHash = ""
    , supportsPointerEvents = True
    }


{-| A key press delivered to the studio the way the browser's keydown reaches it.
-}
pressKey : { key : String, ctrl : Bool, shift : Bool } -> Msg
pressKey { key, ctrl, shift } =
    StudioMsg
        (Core.ViewportMsg
            (Viewport.KeyPressed
                { key = key
                , ctrl = ctrl
                , meta = False
                , alt = False
                , shift = shift
                , repeat = False
                , targetTag = "BODY"
                , targetEditable = False
                , targetRole = ""
                }
            )
        )


{-| Positioning feedback for the one selected gear against its nearest gear.
-}
gearFeedbackTests : Test
gearFeedbackTests =
    let
        place part matrix doc =
            Document.addPlacement "" { part = part, color = 7, matrix = matrix } doc

        -- A 24T at the origin (id `a`) and a second part placed by `matrix` (id `b`).
        pairWith part matrix =
            let
                ( doc1, a ) =
                    place "3648.dat" Mat4.identity Document.empty

                ( doc2, b ) =
                    place part matrix doc1
            in
            ( doc2, a, b )

        relationFor part matrix =
            let
                ( doc, a, _ ) =
                    pairWith part matrix
            in
            GearFeedback.gearFeedback [ a ] doc |> Maybe.map .relation
    in
    describe "Gear positioning feedback"
        [ test "two 24T 40 LDU apart are too close: need 60" <|
            \_ ->
                relationFor "3648.dat" (Mat4.makeTranslate (vec3 40 0 0))
                    |> Expect.equal (Just (GearFeedback.TooClose { distance = 40, required = 60 }))
        , test "two 24T 80 LDU apart are too far" <|
            \_ ->
                relationFor "3648.dat" (Mat4.makeTranslate (vec3 80 0 0))
                    |> Expect.equal (Just (GearFeedback.TooFar { distance = 80, required = 60 }))
        , test "two 24T 60 LDU apart mesh" <|
            \_ ->
                relationFor "3648.dat" (Mat4.makeTranslate (vec3 60 0 0))
                    |> Expect.equal (Just GearFeedback.Meshes)
        , test "8T and 24T mesh at 40 LDU, matching the gear graph" <|
            \_ ->
                let
                    ( doc, a, _ ) =
                        pairWith "3647.dat" (Mat4.makeTranslate (vec3 0 40 0))
                in
                Expect.all
                    [ \_ -> Expect.equal (Just GearFeedback.Meshes) (GearFeedback.gearFeedback [ a ] doc |> Maybe.map .relation)
                    , \_ -> Expect.equal True (Machine.isAccomplished (Machine.analyseMachine doc EmbeddedParts.cache))
                    ]
                    ()
        , test "an axial offset reports not in the same plane" <|
            \_ ->
                relationFor "3648.dat" (Mat4.makeTranslate (vec3 60 0 20))
                    |> Expect.equal (Just (GearFeedback.NotSamePlane { offset = 20 }))
        , test "crossed spur axles are not parallel" <|
            \_ ->
                relationFor "3648.dat" (Mat4.mul (Mat4.makeTranslate (vec3 60 0 0)) (Mat4.makeRotate (pi / 2) (vec3 0 1 0)))
                    |> Expect.equal (Just GearFeedback.NotParallel)
        , test "a lone gear has no other gear" <|
            \_ ->
                let
                    ( doc, a ) =
                        place "3648.dat" Mat4.identity Document.empty
                in
                GearFeedback.gearFeedback [ a ] doc
                    |> Expect.equal (Just { gear = "24T", other = Nothing, relation = GearFeedback.NoOtherGear })
        , test "a meshing partner wins over a nearer gear on the same axle" <|
            \_ ->
                let
                    ( doc1, a, _ ) =
                        pairWith "3648.dat" (Mat4.makeTranslate (vec3 0 0 20))

                    ( doc, _ ) =
                        place "3647.dat" (Mat4.makeTranslate (vec3 40 0 0)) doc1
                in
                GearFeedback.gearFeedback [ a ] doc
                    |> Expect.equal (Just { gear = "24T", other = Just "8T", relation = GearFeedback.Meshes })
        , test "a crown gear with a parallel spur needs right angles" <|
            \_ ->
                let
                    ( doc1, crown ) =
                        place "3650b.dat" Mat4.identity Document.empty

                    ( doc, _ ) =
                        place "3647.dat" (Mat4.makeTranslate (vec3 40 0 0)) doc1
                in
                GearFeedback.gearFeedback [ crown ] doc
                    |> Expect.equal (Just { gear = "24T crown", other = Just "8T", relation = GearFeedback.NotPerpendicular })
        , test "no feedback for a non-gear, an empty or a multiple selection" <|
            \_ ->
                let
                    ( doc, a, b ) =
                        pairWith "3713.dat" Mat4.identity
                in
                Expect.all
                    [ \_ -> Expect.equal Nothing (GearFeedback.gearFeedback [ b ] doc)
                    , \_ -> Expect.equal Nothing (GearFeedback.gearFeedback [] doc)
                    , \_ -> Expect.equal Nothing (GearFeedback.gearFeedback [ a, b ] doc)
                    ]
                    ()
        , test "pill text reads distance and requirement" <|
            \_ ->
                Expect.equal
                    [ "24T ↔ 24T: 40 LDU apart — needs 60"
                    , "Meshes with 8T ✓"
                    , "24T ↔ 24T: not in the same plane (offset 20)"
                    , "24T crown ↔ 8T: axles must be at right angles"
                    ]
                    [ GearFeedback.feedbackText { gear = "24T", other = Just "24T", relation = GearFeedback.TooFar { distance = 40, required = 60 } }
                    , GearFeedback.feedbackText { gear = "24T", other = Just "8T", relation = GearFeedback.Meshes }
                    , GearFeedback.feedbackText { gear = "24T", other = Just "24T", relation = GearFeedback.NotSamePlane { offset = 20 } }
                    , GearFeedback.feedbackText { gear = "24T crown", other = Just "8T", relation = GearFeedback.NotPerpendicular }
                    ]
        ]


{-| Parts added from the dock: gears and axles on one axle line, each on a free
spot, and drags that keep their height.
-}
dockPlacementTests : Test
dockPlacementTests =
    let
        dockItem file =
            [ GearsCategory, BeamsCategory, AxlesCategory, PinsCategory ]
                |> List.concatMap Catalog.partsForCategory
                |> List.filter (\item -> item.file == file)
                |> List.head
                |> Maybe.withDefault gear8

        addFromDock file model =
            update (AddCatalogPart (dockItem file)) model |> Tuple.first

        origins model =
            Core.getPlacements model.studio |> List.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))

        start =
            Tuple.first (init testFlags)

        gearPair =
            start |> addFromDock "3647.dat" |> addFromDock "3649.dat"

        press key model =
            update (pressKey { key = key, ctrl = False, shift = False }) model |> Tuple.first

        centreDistance model =
            case origins model of
                [ a, b ] ->
                    Vec3.distance a b

                _ ->
                    -1

        -- Front view, then A or D (whichever closes the gap) until 60 apart.
        brought =
            let
                front =
                    press "1" gearPair

                towards =
                    if centreDistance (press "a" front) < centreDistance front then
                        "a"

                    else
                        "d"

                go n m =
                    if n == 0 || centreDistance m <= 60 then
                        m

                    else
                        go (n - 1) (press towards m)
            in
            go 5 front

        boxOf model p =
            DragPlane.worldBox p.matrix (Core.partBox p.part model.studio)

        -- Each new part clashes with none of the parts added before it.
        noClashes files =
            List.foldl
                (\file ( model, ok ) ->
                    let
                        next =
                            addFromDock file model

                        placed =
                            Core.getPlacements next.studio
                    in
                    case List.reverse placed of
                        newest :: earlier ->
                            ( next, ok && not (List.any (DragPlane.boxesOverlap (boxOf next newest) << boxOf next) earlier) )

                        [] ->
                            ( next, False )
                )
                ( start, True )
                files
                |> Tuple.second
    in
    describe "Adding parts from the dock"
        [ test "an 8T and a 40T brought to mesh by the keys interlock their teeth" <|
            \_ ->
                let
                    model =
                        brought

                    gears =
                        Detect.extractGears Data.gearParts
                            (Document.toLines model.studio.document)
                            model.studio.viewport.partCache

                    worst =
                        case gears of
                            [ fixed, moving ] ->
                                Phase.meshConstraints { fixed = fixed, moving = moving }
                                    |> Maybe.andThen
                                        (List.map (\c -> max c.fixedFraction (abs c.movingResidual / c.movingPitch))
                                            >> List.minimum
                                        )

                            _ ->
                                Nothing
                in
                Expect.all
                    [ \_ ->
                        GearFeedback.gearFeedback (Set.toList model.studio.selection) model.studio.document
                            |> Maybe.map .relation
                            |> Expect.equal (Just GearFeedback.Meshes)
                    , \_ ->
                        worst
                            |> Maybe.map (\v -> Expect.within (Expect.Absolute 1.0e-6) 0 v)
                            |> Maybe.withDefault (Expect.fail "the gears have no mesh constraints")
                    ]
                    ()
        , test "a new 8T and 40T have their axes at the same height, on the axle line" <|
            \_ ->
                origins gearPair
                    |> List.map Vec3.getY
                    |> Expect.equal [ Placement.axleLineY, Placement.axleLineY ]
        , test "the 40T goes into the 8T's plane, beside it without overlapping" <|
            \_ ->
                case origins gearPair of
                    [ a, b ] ->
                        Expect.all
                            [ \_ -> Expect.equal (Vec3.getZ a) (Vec3.getZ b)
                            , \_ -> Expect.atLeast 64.8 (abs (Vec3.getX b - Vec3.getX a))
                            ]
                            ()

                    other ->
                        Expect.fail ("expected two placements, got " ++ String.fromInt (List.length other))
        , test "a few A/D presses in the front view bring them to mesh exactly (60 LDU)" <|
            \_ ->
                Expect.all
                    [ \m -> Expect.within (Expect.Absolute 1.0e-6) 60 (centreDistance m)
                    , \m -> Expect.equal True (Machine.isAccomplished m.machine)
                    , \m ->
                        GearFeedback.gearFeedback (Set.toList m.studio.selection) m.studio.document
                            |> Maybe.map .relation
                            |> Expect.equal (Just GearFeedback.Meshes)
                    ]
                    brought
        , test "axles, beams and pins go on the axle line too; nothing new overlaps an earlier part" <|
            \_ ->
                let
                    files =
                        [ "3648.dat", "3648.dat", "3705.dat", "32316.dat", "2780.dat", "6590.dat", "3647.dat", "3649.dat" ]
                in
                Expect.all
                    [ \_ -> Expect.equal True (noClashes files)
                    , \_ ->
                        List.foldl addFromDock start files
                            |> origins
                            |> List.map Vec3.getY
                            |> Expect.equal (List.map (\_ -> Placement.axleLineY) files)
                    ]
                    ()
        , test "drags keep the part's height (drag settling is off)" <|
            \_ -> Expect.equal False gearPair.studio.dragSettles
        , test "rowSpot walks +X, then -X, along the start's row" <|
            \_ ->
                let
                    box =
                        { min = vec3 -10 -10 -10, max = vec3 10 10 10 }

                    at x =
                        { min = vec3 (x - 10) -10 -10, max = vec3 (x + 10) 10 10 }
                in
                Expect.all
                    [ \_ -> Expect.equal (Just ( 0, 5 )) (Placement.rowSpot { box = box, start = ( 0, 5 ), step = 20, others = [] })
                    , \_ -> Expect.equal (Just ( 20, 0 )) (Placement.rowSpot { box = box, start = ( 0, 0 ), step = 20, others = [ at 0 ] })
                    , \_ -> Expect.equal (Just ( -20, 0 )) (Placement.rowSpot { box = box, start = ( 0, 0 ), step = 20, others = [ at 0, at 20 ] })
                    ]
                    ()
        , test "a project's own (non-Technic) part rests on the ground" <|
            \_ -> Expect.equal False (Placement.onAxleLine "3001.dat")
        ]


{-| The snapshot modal: Escape closes it and does nothing else, so the part under
the modal keeps its selection.
-}
snapshotTests : Test
snapshotTests =
    let
        start =
            Tuple.first (init testFlags)

        selected =
            update (AddCatalogPart gear8) start
                |> Tuple.first
                |> (\m -> Tuple.first (update (StudioMsg (Core.SelectOnly 0)) m))
    in
    describe "Snapshot modal"
        [ test "Escape closes an open snapshot and keeps the selection" <|
            \_ ->
                let
                    ( opened, _ ) =
                        update CaptureSnapshot selected

                    ( afterEscape, _ ) =
                        update (pressKey { key = "Escape", ctrl = False, shift = False }) opened
                in
                Expect.equal [ True, True, True ]
                    [ opened.snapshotModal /= Nothing
                    , afterEscape.snapshotModal == Nothing
                    , Core.hasSelection afterEscape.studio
                    ]
        , test "progress and a failure reach the open snapshot" <|
            \_ ->
                let
                    ( opened, _ ) =
                        update CaptureSnapshot selected

                    ( progressed, _ ) =
                        update (SnapshotProgress { fraction = 0.25, samples = 4 }) opened

                    ( failed, _ ) =
                        update (SnapshotFailed "Tracer ran out of memory") progressed
                in
                Expect.equal [ True, True ]
                    [ Maybe.map .progress progressed.snapshotModal == Just { fraction = 0.25, samples = 4 }
                    , Maybe.map .phase failed.snapshotModal == Just (SnapshotModal.Failed "Tracer ran out of memory")
                    ]
        ]


{-| The top-right menu: it opens and closes, Escape closes it before it
deselects anything, its items send the messages their buttons used to, and the
dock's category tabs run down the left of the catalog.
-}
menuTests : Test
menuTests =
    let
        start =
            Tuple.first (init testFlags)

        selected =
            update (AddCatalogPart gear8) start
                |> Tuple.first
                |> (\m -> Tuple.first (update (StudioMsg (Core.SelectOnly 0)) m))
    in
    describe "Top-right menu and category tabs"
        [ test "the menu opens, closes, and pauses the editing keys while open" <|
            \_ ->
                let
                    ( opened, _ ) =
                        update ToggleMenu start

                    ( closed, _ ) =
                        update CloseMenu opened
                in
                Expect.equal
                    [ ( False, False ), ( True, True ), ( False, False ) ]
                    [ ( start.menuOpen, start.studio.helpVisible )
                    , ( opened.menuOpen, opened.studio.helpVisible )
                    , ( closed.menuOpen, closed.studio.helpVisible )
                    ]
        , test "Escape closes an open menu and keeps the selection" <|
            \_ ->
                let
                    ( opened, _ ) =
                        update ToggleMenu selected

                    ( afterEscape, _ ) =
                        update (pressKey { key = "Escape", ctrl = False, shift = False }) opened
                in
                Expect.equal [ True, False, True ]
                    [ Core.hasSelection selected.studio
                    , afterEscape.menuOpen
                    , Core.hasSelection afterEscape.studio
                    ]
        , test "Escape with the menu closed still deselects" <|
            \_ ->
                let
                    ( afterEscape, _ ) =
                        update (pressKey { key = "Escape", ctrl = False, shift = False }) selected
                in
                Expect.equal False (Core.hasSelection afterEscape.studio)
        , test "Undo from the menu undoes the last add, as the toolbar button did" <|
            \_ ->
                let
                    ( added, _ ) =
                        update (AddCatalogPart gear8) start

                    ( undone, _ ) =
                        update (MenuPicked Undo) added
                in
                Expect.equal [ 1, 0 ]
                    [ List.length (Core.getPlacements added.studio), List.length (Core.getPlacements undone.studio) ]
        , test "the visible ? button toggles the shortcut list, as the ? key does" <|
            \_ ->
                Query.fromHtml (view start)
                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Keyboard shortcuts (?)") ]
                    |> Event.simulate Event.click
                    |> Event.expect ToggleHelp
        , test "an accomplished machine shows a one-line status banner, with no headline" <|
            \_ ->
                let
                    modelStr =
                        "1 7 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n1 7 30 0 0 1 0 0 0 1 0 0 0 1 4019.dat"

                    ( accomplishedModel, _ ) =
                        init { testFlags | initialHash = "#model=" ++ Url.percentEncode modelStr }
                in
                Query.fromHtml (view accomplishedModel)
                    |> Expect.all
                        [ Query.has [ Selector.attribute (Attr.attribute "role" "status"), Selector.text "All 2 gears are rotating together!" ]
                        , Query.hasNot [ Selector.text "Contraption Accomplished" ]
                        ]
        , test "a machine that is not accomplished shows no banner" <|
            \_ ->
                Query.fromHtml (view start)
                    |> Query.hasNot [ Selector.attribute (Attr.attribute "role" "status") ]
        , test "the open menu lists its actions, and the tabs select a category" <|
            \_ ->
                let
                    ( opened, _ ) =
                        update ToggleMenu (Tuple.first (init (modelFlags pair8x24)))
                in
                Query.fromHtml (view opened)
                    |> Expect.all
                        [ Query.findAll [ Selector.attribute (Attr.attribute "role" "menuitem") ] >> Query.count (Expect.equal 5)
                        , Query.has [ Selector.text "Share link" ]
                        , Query.findAll [ Selector.attribute (Attr.attribute "role" "tab") ] >> Query.count (Expect.equal 4)
                        , Query.has [ Selector.attribute (Attr.attribute "aria-orientation" "vertical") ]
                        ]
        ]


{-| Sharing and toast notification lifecycle tests.
-}
sharingAndToastTests : Test
sharingAndToastTests =
    let
        start =
            Tuple.first (init testFlags)
    in
    describe "Sharing and toast lifecycle"
        [ test "ShareLink sets toast state and closes menu" <|
            \_ ->
                let
                    ( menuOpenModel, _ ) =
                        update ToggleMenu start

                    ( sharedModel, _ ) =
                        update ShareLink menuOpenModel
                in
                Expect.all
                    [ \m -> Expect.equal False m.menuOpen
                    , \m -> Expect.equal (Just "Link copied to clipboard!") (Maybe.map .message m.toast)
                    ]
                    sharedModel
        , test "DismissToast with matching id clears toast; mismatching id preserves toast" <|
            \_ ->
                let
                    ( sharedModel, _ ) =
                        update ShareLink start

                    toastId =
                        sharedModel.toast |> Maybe.map .id |> Maybe.withDefault 0

                    ( mismatchModel, _ ) =
                        update (DismissToast (toastId + 999)) sharedModel

                    ( matchModel, _ ) =
                        update (DismissToast toastId) sharedModel
                in
                Expect.all
                    [ \_ -> Expect.notEqual Nothing mismatchModel.toast
                    , \_ -> Expect.equal Nothing matchModel.toast
                    ]
                    ()
        , test "viewToast renders message with role='status'" <|
            \_ ->
                Query.fromHtml (viewToast "Link copied to clipboard!")
                    |> Expect.all
                        [ Query.has [ Selector.attribute (Attr.attribute "role" "status") ]
                        , Query.has [ Selector.text "Link copied to clipboard!" ]
                        ]
        , test "shareHash shares the default kit when there is no model" <|
            \_ ->
                let
                    hash =
                        shareHash start
                in
                Expect.all
                    [ \h -> Expect.equal True (String.startsWith "parts=" h)
                    , \h -> Expect.equal False (String.contains "model=" h)
                    ]
                    hash
        ]


{-| Touch layout: the shared camera globe (its drag orbits the camera), the
phone-width dock/camera toggle and the names shown where tooltips never show.
-}
touchTests : Test
touchTests =
    let
        start =
            Tuple.first (init (modelFlags pair8x24))

        at width model =
            update (WindowResized width 844) model |> Tuple.first

        toggled model =
            update ToggleCameraPanel model |> Tuple.first

        expectCount n label model =
            Query.fromHtml (view model)
                |> Query.findAll [ Selector.attribute (Attr.attribute "aria-label" label) ]
                |> Query.count (Expect.equal n)

        azimuth model =
            model.studio.viewport.camera.azimuth
    in
    describe "Touch layout"
        [ test "a wide window shows the parts dock and the camera globe side by side" <|
            \_ ->
                let
                    wide =
                        at 964 start
                in
                Expect.all
                    [ \_ -> expectCount 1 "Parts" wide
                    , \_ -> expectCount 1 "Camera" wide
                    , \_ -> expectCount 0 "Camera controls" wide
                    ]
                    ()
        , test "a phone shows the parts dock first, and the camera globe once the toggle is pressed" <|
            \_ ->
                let
                    phone =
                        at 390 start
                in
                Expect.all
                    [ \_ -> expectCount 1 "Parts" phone
                    , \_ -> expectCount 0 "Camera" phone
                    , \_ -> expectCount 0 "Parts" (toggled phone)
                    , \_ -> expectCount 1 "Camera" (toggled phone)
                    ]
                    ()
        , test "the phone toggle reports whether the camera is shown" <|
            \_ ->
                let
                    toggle model =
                        Query.fromHtml (view model)
                            |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Camera controls") ]
                in
                Expect.all
                    [ \_ -> toggle (at 390 start) |> Query.has [ Selector.attribute (Attr.attribute "aria-pressed" "false") ]
                    , \_ -> toggle (toggled (at 390 start)) |> Query.has [ Selector.attribute (Attr.attribute "aria-pressed" "true") ]
                    ]
                    ()
        , test "dragging the globe orbits the camera" <|
            \_ ->
                let
                    pressed =
                        update (GlobePressed ( 0, 0 )) start |> Tuple.first

                    moved =
                        update (GlobeMoved ( 22, 0 )) pressed |> Tuple.first
                in
                Expect.greaterThan 1.0e-6 (abs (azimuth moved - azimuth start))
        , test "a dock part tap records its name for touch screens" <|
            \_ ->
                Expect.equal (Just gear8.name) (update (AddCatalogPart gear8) start |> Tuple.first |> .lastTapped)
        , test "the touch label appears only once a part has been tapped" <|
            \_ ->
                Expect.all
                    [ \_ -> Query.fromHtml (view start) |> Query.findAll [ Selector.class "touch-only" ] |> Query.count (Expect.equal 0)
                    , \_ -> Query.fromHtml (view (update (AddCatalogPart gear8) start |> Tuple.first)) |> Query.findAll [ Selector.class "touch-only" ] |> Query.count (Expect.greaterThan 0)
                    ]
                    ()
        ]


{-| The accomplishment banner states the gear count in its text.
-}
bannerTests : Test
bannerTests =
    describe "Accomplishment banner"
        [ test "the banner renders its text" <|
            \_ ->
                Query.fromHtml (accomplishmentBanner 2)
                    |> Query.has [ Selector.text "All 2 gears are rotating together!" ]
        ]


{-| The Fine toggle in the action bar: it shows with or without a selection,
reads its state with `aria-pressed`, and survives selection changes.
-}
fineTests : Test
fineTests =
    let
        start =
            Tuple.first (init testFlags)

        fineButtons model =
            Query.fromHtml (view model)
                |> Query.findAll [ Selector.attribute (Attr.attribute "aria-label" "Fine moves") ]

        fineButton model =
            Query.fromHtml (view model)
                |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Fine moves") ]
    in
    describe "Fine toggle"
        [ test "is on the bar with no selection, off by default" <|
            \_ ->
                Expect.all
                    [ \_ -> fineButtons start |> Query.count (Expect.equal 1)
                    , \_ -> fineButton start |> Query.has [ Selector.attribute (Attr.attribute "aria-pressed" "false") ]
                    ]
                    ()
        , test "a press switches it on, and only the toggle shows without a selection" <|
            \_ ->
                let
                    on =
                        Tuple.first (update ToggleFine start)
                in
                Expect.all
                    [ \_ -> fineButton on |> Query.has [ Selector.attribute (Attr.attribute "aria-pressed" "true") ]
                    , \_ -> Query.fromHtml (view on) |> Query.findAll [ Selector.attribute (Attr.attribute "aria-label" "Turn left") ] |> Query.count (Expect.equal 0)
                    , \_ -> Expect.equal True (Core.fineMove on.studio)
                    ]
                    ()
        , test "it persists when the selection is cleared, and a second press switches it off" <|
            \_ ->
                let
                    cleared =
                        Tuple.first (update ToggleFine start)
                            |> (\m -> Tuple.first (update (StudioMsg Core.ClearSelection) m))
                in
                Expect.all
                    [ \_ -> Expect.equal True (Core.fineMove cleared.studio)
                    , \_ -> Expect.equal False (Core.fineMove (Tuple.first (update ToggleFine cleared)).studio)
                    ]
                    ()
        ]


{-| Technic mode: how it is derived, what it switches on, the Space key and the
play button, the dock, placement, and the examples from docs/examples.md.
-}
technicTests : Test
technicTests =
    let
        start =
            Tuple.first (init testFlags)

        withGears =
            Tuple.first (init (modelFlags pair8x24))

        space =
            pressKey { key = " ", ctrl = False, shift = False }

        add item model =
            update (AddPart item) model |> Tuple.first

        playButtons model =
            Query.fromHtml (view model)
                |> Query.findAll [ Selector.attribute (Attr.title "Play / pause (Space)") ]

        tabs model =
            Query.fromHtml (view model)
                |> Query.findAll [ Selector.attribute (Attr.attribute "role" "tab") ]
    in
    describe "Technic mode"
        [ test "is off for an empty document and the default kit" <|
            \_ ->
                Expect.equal False (technicMode start)
        , test "is on for a document with two meshing gears" <|
            \_ ->
                Expect.equal True (technicMode withGears)
        , test "is off again once the document has no gear" <|
            \_ ->
                Expect.equal False (technicMode (update Clear withGears |> Tuple.first))
        , test "is on for a URL kit that lists a gear, with no gear placed" <|
            \_ ->
                Expect.equal True (technicMode (Tuple.first (init { testFlags | initialHash = "#parts=3648.dat:7:1" })))
        , test "is off for a URL kit with no gear" <|
            \_ ->
                Expect.equal False (technicMode (Tuple.first (init { testFlags | initialHash = "#parts=3001.dat:14:4" })))
        , test "Space is unbound without gears: the machine does not play" <|
            \_ ->
                let
                    model =
                        update space start |> Tuple.first
                in
                Expect.equal ( False, 0 ) ( model.playing, model.currentTime )
        , test "Space plays and pauses the machine with gears" <|
            \_ ->
                let
                    played =
                        update space withGears |> Tuple.first

                    paused =
                        update space played |> Tuple.first
                in
                Expect.equal [ True, False ] [ played.playing, paused.playing ]
        , test "the play button is hidden without gears and shown with them" <|
            \_ ->
                Expect.all
                    [ \_ -> playButtons start |> Query.count (Expect.equal 0)
                    , \_ -> playButtons withGears |> Query.count (Expect.equal 1)
                    ]
                    ()
        , test "a running machine stops and rewinds when the edit ends Technic mode" <|
            \_ ->
                let
                    playing =
                        withGears
                            |> update TogglePlay
                            |> Tuple.first
                            |> update (Tick 500)
                            |> Tuple.first

                    cleared =
                        update Clear playing |> Tuple.first
                in
                Expect.equal ( True, False, 0 ) ( playing.playing, cleared.playing, cleared.currentTime )
        , test "gear alignment and drag settling follow Technic mode" <|
            \_ ->
                let
                    cleared =
                        update Clear withGears |> Tuple.first
                in
                Expect.equal
                    [ ( True, False ), ( False, True ), ( False, True ) ]
                    [ ( withGears.studio.gearAlignment, withGears.studio.dragSettles )
                    , ( cleared.studio.gearAlignment, cleared.studio.dragSettles )
                    , ( start.studio.gearAlignment, start.studio.dragSettles )
                    ]
        , test "the dock is the Technic catalog, with its tabs, when there is no URL kit" <|
            \_ ->
                Expect.all
                    [ \_ -> tabs withGears |> Query.count (Expect.equal 4)
                    , \_ -> Query.fromHtml (view withGears) |> Query.has [ Selector.attribute (Attr.attribute "aria-label" "Parts") ]
                    ]
                    ()
        , test "a URL kit adds a Project tab first, selected, before the catalog tabs" <|
            \_ ->
                let
                    kit =
                        Tuple.first (init { testFlags | initialHash = "#parts=3648.dat:7:1,6590.dat:7:2" })
                in
                Expect.all
                    [ \_ -> tabs kit |> Query.count (Expect.equal 5)
                    , \_ -> tabs kit |> Query.first |> Query.has [ Selector.text "Project", Selector.attribute (Attr.attribute "aria-selected" "true") ]
                    , \_ -> Query.fromHtml (view kit) |> Query.has [ Selector.attribute (Attr.attribute "aria-label" "Parts") ]
                    , \_ -> Query.fromHtml (view kit) |> Query.hasNot [ Selector.attribute (Attr.attribute "aria-label" "Kit") ]
                    ]
                    ()
        , test "a Technic kit part is unlimited: the Project tab adds it past its count" <|
            \_ ->
                let
                    item =
                        Catalog.fromKitItem { part = "3649.dat", color = 7, count = 2 }

                    model =
                        Tuple.first (init { testFlags | initialHash = "#parts=3649.dat:7:2" })
                            |> update (AddCatalogPart item)
                            |> Tuple.first
                            |> update (AddCatalogPart item)
                            |> Tuple.first
                            |> update (AddCatalogPart item)
                            |> Tuple.first
                in
                Expect.equal 3 (List.length (Core.getPlacements model.studio))
        , test "a Technic link keeps its kit as parts=; a catalog dock shares the model alone" <|
            \_ ->
                let
                    kitLink =
                        Tuple.first (init { testFlags | initialHash = "#model=" ++ Url.percentEncode pair8x24 ++ "&parts=3648.dat%3A7%3A1" })
                in
                Expect.all
                    [ \_ -> Expect.equal False (String.contains "parts=" (shareHash withGears))
                    , \_ -> Expect.equal True (String.contains "parts=3648.dat%3A7%3A1" (shareHash kitLink))
                    ]
                    ()
        , test "a gear added from the catalog turns the alignment on and goes on the axle line" <|
            \_ ->
                let
                    model =
                        update (AddCatalogPart gear8) start |> Tuple.first
                in
                Expect.all
                    [ \_ -> Expect.equal True model.studio.gearAlignment
                    , \_ ->
                        Core.getPlacements model.studio
                            |> List.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0)))
                            |> Expect.equal [ Placement.axleLineY ]
                    ]
                    ()
        , test "the Contraptions examples from the docs load in Technic mode, with their kit and gears" <|
            \_ ->
                List.map
                    (\hash ->
                        let
                            model =
                                Tuple.first (init { testFlags | initialHash = hash })
                        in
                        ( technicMode model, model.kitFromUrl, List.length model.machine.gears )
                    )
                    [ twinGearboxHash, speedReducerHash, gearTrainHash ]
                    |> Expect.equal [ ( True, True, 1 ), ( True, True, 1 ), ( True, True, 2 ) ]
        , test "the Twin 24T and 3-Gear Train examples list their 24T in the Project tab, and it adds a second 24T" <|
            \_ ->
                let
                    kit24 =
                        Catalog.fromKitItem { part = "3648.dat", color = 7, count = 1 }

                    count24 model =
                        List.length (List.filter (\p -> p.part == "3648.dat") (Core.getPlacements model.studio))

                    starts =
                        List.map (\hash -> Tuple.first (init { testFlags | initialHash = hash })) [ twinGearboxHash, gearTrainHash ]
                in
                Expect.all
                    [ \_ -> Expect.all (List.map (\m _ -> Query.fromHtml (view m) |> Query.has [ Selector.attribute (Attr.attribute "aria-label" "Add 24T Gear (3648)") ]) starts) ()
                    , \_ -> List.map (\m -> count24 (Tuple.first (update (AddCatalogPart kit24) m))) starts |> Expect.equal [ 2, 2 ]
                    ]
                    ()
        , test "the Contraptions 3-gear train example is accomplished as it loads" <|
            \_ ->
                Tuple.first (init { testFlags | initialHash = gearTrainHash })
                    |> .machine
                    |> Machine.isAccomplished
                    |> Expect.equal True
        , test "the Brick Play cottage example from the docs loads with Technic mode off" <|
            \_ ->
                let
                    model =
                        Tuple.first (init { testFlags | initialHash = cottageHash })
                in
                Expect.equal ( False, True ) ( technicMode model, model.kitFromUrl )
        ]


{-| The Brick Contraptions examples in docs/examples.md (Production links), which
open in Brick Play in Technic mode.
-}
twinGearboxHash : String
twinGearboxHash =
    "#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203648.dat%0A1%207%200%20-60%2012%201%200%200%200%201%200%200%200%201%206590.dat&parts=3648.dat%3A7%3A1%2C6590.dat%3A7%3A2"


speedReducerHash : String
speedReducerHash =
    "#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203647.dat%0A1%207%200%20-60%2012%201%200%200%200%201%200%200%200%201%206590.dat&parts=3649.dat%3A7%3A1%2C6590.dat%3A7%3A2"


gearTrainHash : String
gearTrainHash =
    "#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%20-40%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%20-40%20-60%200%201%200%200%200%201%200%200%200%201%203647.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203648.dat&parts=3648.dat%3A7%3A1%2C4019.dat%3A7%3A1%2C6590.dat%3A7%3A3"


{-| The Brick Play cottage example in docs/examples.md (Production link).
-}
cottageHash : String
cottageHash =
    "#model=1%2019%200%20-24%200%201%200%200%200%201%200%200%200%201%203001.dat%0A1%2015%200%20-72%200%201%200%200%200%201%200%200%200%201%2060592.dat%0A1%2019%20-30%20-48%200%201%200%200%200%201%200%200%200%201%203005.dat%0A1%2019%2030%20-48%200%201%200%200%200%201%200%200%200%201%203005.dat&parts=3001.dat%3A19%3A3%2C3005.dat%3A19%3A6%2C3001.dat%3A14%3A2%2C3039.dat%3A4%3A4%2C60592.dat%3A15%3A1%2C3004.dat%3A19%3A4"
