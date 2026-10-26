module Studio.HealthTest exposing (suite)

{-| Tests for Tidy up's build health (`Studio.Health`): stud misalignment, the
jumper and Technic exemptions, overlaps, glue, determinism, and the studio's
`applyFixes` (one undo step, glued parts refused).
-}

import Dict
import Expect
import LDraw.Document as Document exposing (Document)
import LDraw.Parser as Parser
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Writer as Writer
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3
import Render.Instanced as Instanced
import Set
import Studio.Core as Core
import Studio.Edit as Edit exposing (Command(..))
import Studio.Health as Health exposing (FixReason(..), Issue(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Studio.Health (Tidy up)"
        [ misalignmentTests
        , overlapTests
        , technicTests
        , glueTests
        , planTests
        , coreTests
        , ghostTests
        ]


{-| The Tidy up preview's ghosts: each moved part is drawn where the fix puts it,
through the same document-to-render conversion as the live scene, and nothing
else is drawn again.
-}
ghostTests : Test
ghostTests =
    describe "preview ghosts (Core.previewScene)"
        [ test "a ghost is drawn where the planned matrix puts the part, not mirrored" <|
            \_ ->
                let
                    fromDoc =
                        Mat4.makeTranslate3 0 -72 0

                    toDoc =
                        Mat4.makeTranslate3 5 -72 0

                    fix =
                        { placement = 0, from = fromDoc, to = toDoc, reason = SnapToStud }

                    scene =
                        Instanced.buildFrom jumperCache [ ( 0, { part = "3794a.dat", color = 14, matrix = fromDoc } ) ]

                    expected =
                        Instanced.build jumperCache [ { part = "3794a.dat", color = 14, matrix = toDoc } ]

                    ghosts =
                        Core.previewScene [ fix ] scene
                in
                Expect.all
                    [ \_ ->
                        Expect.equal
                            (List.map (.matrix >> Mat4.toRecord) expected.placements)
                            (List.map (.matrix >> Mat4.toRecord) ghosts.placements)
                    , \_ ->
                        case ghosts.placements of
                            [ ghost ] ->
                                Expect.equal 72 (Vec3.getY (Mat4.transform ghost.matrix (Vec3.vec3 0 0 0)))

                            other ->
                                Expect.fail ("expected one ghost, got " ++ String.fromInt (List.length other))
                    ]
                    ()
        , test "only the moved parts are ghosted, and no proxy layer repeats the scene" <|
            \_ ->
                let
                    fromDoc =
                        Mat4.makeTranslate3 0 -72 0

                    fix =
                        { placement = 0, from = fromDoc, to = Mat4.makeTranslate3 5 -72 0, reason = SnapToStud }

                    scene =
                        Instanced.buildFrom jumperCache
                            [ ( 0, { part = "3794a.dat", color = 14, matrix = fromDoc } )
                            , ( 1, { part = "3794a.dat", color = 14, matrix = Mat4.makeTranslate3 40 -72 0 } )
                            ]

                    ghosts =
                        Core.previewScene [ fix ] scene
                in
                Expect.all
                    [ \_ -> Expect.equal [ 0 ] (List.map .source ghosts.placements)
                    , \_ -> Expect.equal [] ghosts.proxyChunks
                    , \_ -> Expect.equal True (Dict.isEmpty ghosts.chunkIndex)
                    ]
                    ()
        ]


misalignmentTests : Test
misalignmentTests =
    describe "stud rule"
        [ test "a half-stud-off 1 x 1 on a 2 x 4 is found and moved onto a stud at +-10" <|
            \_ ->
                let
                    doc =
                        lines [ brick "3001.dat" 0 -24 0, brick "3005.dat" 5 -48 0 ]

                    issues =
                        Health.check noParts doc Set.empty

                    fixes =
                        Health.plan noParts doc Set.empty issues
                in
                Expect.all
                    [ \_ -> Expect.equal [ Misaligned { placement = 1 } ] issues
                    , \_ -> Expect.equal 1 (List.length fixes)
                    , \_ ->
                        case fixes of
                            [ fix ] ->
                                Expect.all
                                    [ \f -> Expect.equal 1 f.placement
                                    , \f -> Expect.equal SnapToStud f.reason
                                    , \f -> Expect.equal ( 10, 10 ) ( round (originX f.to), round (abs (originZ f.to)) )
                                    , \f -> Expect.equal -10 (round (originZ f.to))
                                    ]
                                    fix

                            _ ->
                                Expect.fail "expected one fix"
                    ]
                    ()
        , test "an aligned stack is healthy" <|
            \_ ->
                lines [ brick "3001.dat" 0 -24 0, brick "3005.dat" 10 -48 10 ]
                    |> (\doc -> Expect.equal [] (Health.check noParts doc Set.empty))
        , test "a part on a jumper plate whose anti-studs sit on its stud is healthy" <|
            \_ ->
                let
                    doc =
                        lines
                            [ "1 14 0 -16 0 1 0 0 0 1 0 0 0 1 3794a.dat"
                            , brick "3005.dat" 0 -40 0
                            ]
                in
                Expect.equal [] (Health.check jumperCache doc Set.empty)
        , test "a 1 x 1 a few LDU off the lattice on the ground is moved onto it" <|
            \_ ->
                let
                    doc =
                        lines [ brick "3005.dat" 5 -24 0 ]

                    fixes =
                        Health.plan noParts doc Set.empty (Health.check noParts doc Set.empty)
                in
                case fixes of
                    [ fix ] ->
                        Expect.equal ( 10, 10 ) ( round (originX fix.to), round (originZ fix.to) )

                    _ ->
                        Expect.fail "expected one ground fix"
        , test "a floating part with nothing under it is not reported" <|
            \_ ->
                lines [ brick "3005.dat" 5 -80 0 ]
                    |> (\doc -> Expect.equal [] (Health.check noParts doc Set.empty))
        ]


overlapTests : Test
overlapTests =
    describe "overlaps"
        [ test "two overlapping 2 x 4 bricks give one overlap issue" <|
            \_ ->
                let
                    doc =
                        lines [ brick "3001.dat" 0 -24 0, brick "3001.dat" 0 -24 20 ]
                in
                Expect.equal [ Overlapping { first = 0, second = 1 } ] (Health.check noParts doc Set.empty)
        , test "one fix moves the later part, and the moved part overlaps nothing" <|
            \_ ->
                let
                    doc =
                        lines [ brick "3001.dat" 0 -24 0, brick "3001.dat" 0 -24 20 ]

                    fixes =
                        Health.plan noParts doc Set.empty (Health.check noParts doc Set.empty)
                in
                case fixes of
                    [ fix ] ->
                        Expect.all
                            [ \_ -> Expect.equal 1 fix.placement
                            , \_ -> Expect.equal (LiftOut { other = 0 }) fix.reason
                            , \_ -> Expect.equal [] (Health.check noParts (documentWith doc fix) Set.empty)
                            ]
                            ()

                    _ ->
                        Expect.fail "expected one fix"
        , test "bricks that only touch side by side are fine" <|
            \_ ->
                lines [ brick "3001.dat" 0 -24 0, brick "3001.dat" 80 -24 0 ]
                    |> (\doc -> Expect.equal [] (Health.check noParts doc Set.empty))
        ]


technicTests : Test
technicTests =
    describe "Technic connections"
        [ test "an axle through a gear is not an overlap" <|
            \_ ->
                lines
                    [ "1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3648.dat"
                    , "1 0 0 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat"
                    ]
                    |> (\doc -> Expect.equal [] (Health.check noParts doc Set.empty))
        , test "an axle through a beam's holes, with a gear beside the beam, is not an overlap" <|
            \_ ->
                lines
                    [ "1 1 0 -60 -20 0 0 -1 0 1 0 1 0 0 32524.dat"
                    , "1 0 0 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat"
                    , "1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3648.dat"
                    ]
                    |> (\doc -> Expect.equal [] (Health.check noParts doc Set.empty))
        , test "a brick through the same gear is an overlap (control)" <|
            \_ ->
                let
                    doc =
                        lines
                            [ "1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3648.dat"
                            , brick "3001.dat" 0 -60 0
                            ]
                in
                Expect.equal True (List.any isOverlap (Health.check noParts doc Set.empty))
        , test "a Technic part is exempt from the stud rule" <|
            \_ ->
                lines [ "1 7 5 -24 0 1 0 0 0 1 0 0 0 1 3648.dat" ]
                    |> (\doc -> Expect.equal [] (Health.check noParts doc Set.empty))
        ]


glueTests : Test
glueTests =
    describe "glue"
        [ test "a glued part is never in a fix" <|
            \_ ->
                let
                    doc =
                        lines [ brick "3001.dat" 0 -24 0, brick "3001.dat" 0 -24 20 ]

                    fixes =
                        Health.plan noParts doc (Set.fromList [ 0 ]) (Health.check noParts doc (Set.fromList [ 0 ]))
                in
                Expect.equal [ 1 ] (List.map .placement fixes)
        , test "a glued misaligned part is reported but not moved" <|
            \_ ->
                let
                    doc =
                        lines [ brick "3001.dat" 0 -24 0, brick "3005.dat" 5 -48 0 ]

                    glued =
                        Set.fromList [ 1 ]
                in
                Expect.all
                    [ \_ -> Expect.equal [ Misaligned { placement = 1 } ] (Health.check noParts doc glued)
                    , \_ -> Expect.equal [] (Health.plan noParts doc glued (Health.check noParts doc glued))
                    ]
                    ()
        ]


planTests : Test
planTests =
    describe "plan"
        [ test "plans are deterministic" <|
            \_ ->
                let
                    doc =
                        lines [ brick "3001.dat" 0 -24 0, brick "3005.dat" 5 -48 0, brick "3001.dat" 0 -24 20 ]

                    plannedOnce =
                        Health.plan noParts doc Set.empty (Health.check noParts doc Set.empty)

                    plannedTwice =
                        Health.plan noParts doc Set.empty (Health.check noParts doc Set.empty)
                in
                Expect.equal (List.map sample plannedOnce) (List.map sample plannedTwice)
        , test "describe names the move in words" <|
            \_ ->
                let
                    doc =
                        lines [ brick "3001.dat" 0 -24 0, brick "3005.dat" 5 -48 0 ]

                    fixes =
                        Health.plan noParts doc Set.empty (Health.check noParts doc Set.empty)
                in
                Expect.equal [ "Move 1 x 1 brick half a stud" ]
                    (List.map (Health.describe (\_ _ -> "1 x 1 brick") doc) fixes)
        , test "describe names a lift out of another brick" <|
            \_ ->
                let
                    doc =
                        lines [ brick "3001.dat" 0 -24 0, brick "3001.dat" 0 -24 20 ]

                    fixes =
                        Health.plan noParts doc Set.empty (Health.check noParts doc Set.empty)
                in
                Expect.equal [ "Lift 2 x 4 brick out of another brick" ]
                    (List.map (Health.describe (\_ _ -> "2 x 4 brick") doc) fixes)
        ]


coreTests : Test
coreTests =
    describe "Core.applyFixes"
        [ test "applies every fix as one undo step" <|
            \_ ->
                let
                    text =
                        [ brick "3001.dat" 0 -24 0, brick "3005.dat" 5 -48 0, brick "3001.dat" 0 -24 20 ]

                    model =
                        coreWith (String.join "\n" text)

                    doc =
                        model.document

                    fixes =
                        Health.plan model.viewport.partCache doc Set.empty (Health.check model.viewport.partCache doc Set.empty)

                    ( applied, _, _ ) =
                        Core.applyFixes fixes model

                    ( undone, _, _ ) =
                        Core.undo applied
                in
                Expect.all
                    [ \_ -> Expect.notEqual (Writer.writeDocument doc) (Writer.writeDocument applied.document)
                    , \_ -> Expect.equal False (Core.canUndo undone)
                    , \_ -> Expect.equal (Writer.writeDocument doc) (Writer.writeDocument undone.document)
                    , \_ -> Expect.equal [] applied.preview
                    ]
                    ()
        , test "a fix for a glued placement is refused" <|
            \_ ->
                let
                    text =
                        [ "0 !PALIKKA GLUED", brick "3001.dat" 0 -24 0 ]

                    model =
                        coreWith (String.join "\n" text)

                    before =
                        Writer.writeDocument model.document

                    forced =
                        { placement = 0
                        , from = Mat4.identity
                        , to = Mat4.makeTranslate3 0 -40 0
                        , reason = SnapToStud
                        }

                    ( applied, _, _ ) =
                        Core.applyFixes [ forced ] model
                in
                Expect.equal before (Writer.writeDocument applied.document)
        ]


{-| The document of a model built from LDraw lines (one part per line).
-}
lines : List String -> Document
lines text =
    Document.parse (String.join "\n" text)


{-| A placement line for a part at a position, unrotated, in colour 14.
-}
brick : String -> Float -> Float -> Float -> String
brick file x y z =
    "1 14 "
        ++ String.join " " (List.map String.fromFloat [ x, y, z ])
        ++ " 1 0 0 0 1 0 0 0 1 "
        ++ file


noParts : PartCache
noParts =
    Dict.empty


{-| A cache holding the jumper plate 3794a, as its LDraw file gives it: a stud at
the origin, an anti-stud 4 LDU in, and a top and bottom face.
-}
jumperCache : PartCache
jumperCache =
    Dict.fromList
        [ ( "3794a.dat"
          , Loaded
                (Parser.parseFile
                    (String.join "\n"
                        [ "1 16 0 4 0 1 0 0 0 -1 0 0 0 1 stud3.dat"
                        , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud2.dat"
                        , "4 16 -20 0 -10 20 0 -10 20 0 10 -20 0 10"
                        , "4 16 -20 8 -10 20 8 -10 20 8 10 -20 8 10"
                        ]
                    )
                )
          )
        ]


{-| The model of a document made from `text`, with nothing selected.
-}
coreWith : String -> Core.Model
coreWith text =
    let
        config =
            { ldrawBase = "/ldraw"
            , ldrawFallbackBase = ""
            , defaultModelUrl = ""
            , initialHash = ""
            , supportsPointerEvents = True
            , document = Document.parse text
            , gridStep = Just 20
            , rotateStep = Just 90
            , snapDistance = Just 24.0
            , collisionMode = Nothing
            }

        ( model, _ ) =
            Core.init config
    in
    model


{-| The document with one fix applied to its placement, for "no overlap left" checks.
-}
documentWith : Document -> Health.Fix -> Document
documentWith doc fix =
    Tuple.first (Edit.apply (SetMatrix [ ( fix.placement, fix.to ) ]) doc)


isOverlap : Issue -> Bool
isOverlap issue =
    case issue of
        Overlapping _ ->
            True

        Misaligned _ ->
            False


originX : Mat4 -> Float
originX matrix =
    Vec3.getX (Mat4.transform matrix (Vec3.vec3 0 0 0))


originZ : Mat4 -> Float
originZ matrix =
    Vec3.getZ (Mat4.transform matrix (Vec3.vec3 0 0 0))


{-| A matrix as numbers (its image of the origin and of the unit axes), for
comparisons that do not depend on how the matrix is stored.
-}
sample : Health.Fix -> List Float
sample fix =
    List.concatMap
        (\v ->
            let
                p =
                    Mat4.transform fix.to v
            in
            [ Vec3.getX p, Vec3.getY p, Vec3.getZ p ]
        )
        [ Vec3.vec3 0 0 0, Vec3.vec3 1 0 0, Vec3.vec3 0 1 0, Vec3.vec3 0 0 1 ]
        ++ [ toFloat fix.placement ]
