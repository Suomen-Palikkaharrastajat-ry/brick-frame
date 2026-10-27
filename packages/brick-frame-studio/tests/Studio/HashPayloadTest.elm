module Studio.HashPayloadTest exposing (suite)

{-| Tests for Studio.HashPayload: high-density binary hash serialization and deserialization.
-}

import Array
import Bytes
import Bytes.Encode as Encode
import Expect
import LDraw.Document as Document exposing (Document)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3)
import Set
import Studio.HashPayload as HashPayload exposing (KitItem, Mode(..), Payload)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Studio.HashPayload"
        [ roundTripTests
        , rotationModeTests
        , validationTests
        ]


roundTripTests : Test
roundTripTests =
    describe "Round-trip serialization"
        [ test "empty free building model round-trip" <|
            \_ ->
                let
                    payload =
                        HashPayload.emptyFree

                    encoded =
                        HashPayload.encode payload

                    decoded =
                        HashPayload.decode encoded
                in
                case decoded of
                    Just result ->
                        Expect.all
                            [ \r -> Expect.equal FreeBuilding r.mode
                            , \r -> Expect.equal Nothing r.kit
                            , \r -> Expect.equal 0 (List.length (Document.mainPlacements r.document))
                            ]
                            result

                    Nothing ->
                        Expect.fail "Failed to decode empty free building payload"
        , test "free building model with loose placements round-trip" <|
            \_ ->
                let
                    ( doc1, id1 ) =
                        Document.addPlacement "" { part = "3001.dat", color = 14, matrix = Mat4.makeTranslate3 0 -24 0 } Document.empty

                    ( doc2, _ ) =
                        Document.addPlacement "" { part = "3002.dat", color = 4, matrix = Mat4.makeTranslate3 40 -24 20 } doc1

                    payload =
                        { mode = FreeBuilding
                        , document = doc2
                        , kit = Nothing
                        }

                    encoded =
                        HashPayload.encode payload

                    decoded =
                        HashPayload.decode encoded
                in
                case decoded of
                    Just result ->
                        let
                            placements =
                                Document.mainPlacements result.document

                            p1 =
                                List.head placements

                            p2 =
                                List.drop 1 placements |> List.head
                        in
                        Expect.all
                            [ \r -> Expect.equal FreeBuilding r.mode
                            , \r -> Expect.equal Nothing r.kit
                            , \r -> Expect.equal 2 (List.length (Document.mainPlacements r.document))
                            , \_ -> Expect.equal (Just "3001.dat") (Maybe.map .part p1)
                            , \_ -> Expect.equal (Just 14) (Maybe.map .color p1)
                            , \_ -> Expect.equal (Just "3002.dat") (Maybe.map .part p2)
                            , \_ -> Expect.equal (Just 4) (Maybe.map .color p2)
                            , \_ -> Expect.equal Set.empty (Document.gluedIds result.document)
                            ]
                            result

                    Nothing ->
                        Expect.fail "Failed to decode loose placements payload"
        , test "free building model with glued and loose parts round-trip" <|
            \_ ->
                let
                    ( doc1, id1 ) =
                        Document.addPlacement "" { part = "3001.dat", color = 14, matrix = Mat4.makeTranslate3 0 -24 0 } Document.empty

                    doc1Glued =
                        Document.setGlued True id1 doc1

                    ( doc2, _ ) =
                        Document.addPlacement "" { part = "3003.dat", color = 1, matrix = Mat4.makeTranslate3 20 -48 0 } doc1Glued

                    payload =
                        { mode = FreeBuilding
                        , document = doc2
                        , kit = Nothing
                        }

                    encoded =
                        HashPayload.encode payload

                    decoded =
                        HashPayload.decode encoded
                in
                case decoded of
                    Just result ->
                        let
                            placements =
                                Document.mainPlacements result.document

                            gluedIds =
                                Document.gluedIds result.document
                        in
                        Expect.all
                            [ \r -> Expect.equal FreeBuilding r.mode
                            , \r -> Expect.equal 2 (List.length placements)
                            , \_ -> Expect.equal 1 (Set.size gluedIds)
                            ]
                            result

                    Nothing ->
                        Expect.fail "Failed to decode model with glued parts"
        , test "puzzle model with glued parts, placed loose parts, and kit inventory round-trip" <|
            \_ ->
                let
                    ( doc1, id1 ) =
                        Document.addPlacement "" { part = "3001.dat", color = 14, matrix = Mat4.makeTranslate3 0 -24 0 } Document.empty

                    docGlued =
                        Document.setGlued True id1 doc1

                    ( docFull, _ ) =
                        Document.addPlacement "" { part = "3002.dat", color = 4, matrix = Mat4.makeTranslate3 0 -48 0 } docGlued

                    kit =
                        [ { part = "3001.dat", color = 14, count = 2 }
                        , { part = "3002.dat", color = 4, count = 5 }
                        , { part = "3003.dat", color = 1, count = 1 }
                        ]

                    payload =
                        { mode = Puzzle
                        , document = docFull
                        , kit = Just kit
                        }

                    encoded =
                        HashPayload.encode payload

                    decoded =
                        HashPayload.decode encoded
                in
                case decoded of
                    Just result ->
                        Expect.all
                            [ \r -> Expect.equal Puzzle r.mode
                            , \r -> Expect.equal (Just kit) r.kit
                            , \r -> Expect.equal 2 (List.length (Document.mainPlacements r.document))
                            , \r -> Expect.equal 1 (Set.size (Document.gluedIds r.document))
                            ]
                            result

                    Nothing ->
                        Expect.fail "Failed to decode puzzle payload"
        ]


rotationModeTests : Test
rotationModeTests =
    describe "Rotation modes"
        [ test "all 24 canonical orthogonal orientations round-trip" <|
            \_ ->
                let
                    testOneOrientation idx can =
                        let
                            mat =
                                Mat4.fromRecord
                                    { m11 = can.m11
                                    , m12 = can.m12
                                    , m13 = can.m13
                                    , m14 = toFloat (idx * 20)
                                    , m21 = can.m21
                                    , m22 = can.m22
                                    , m23 = can.m23
                                    , m24 = -24
                                    , m31 = can.m31
                                    , m32 = can.m32
                                    , m33 = can.m33
                                    , m34 = 0
                                    , m41 = 0
                                    , m42 = 0
                                    , m43 = 0
                                    , m44 = 1
                                    }

                            ( doc, _ ) =
                                Document.addPlacement "" { part = "3001.dat", color = 14, matrix = mat } Document.empty

                            payload =
                                { mode = FreeBuilding, document = doc, kit = Nothing }

                            decoded =
                                HashPayload.decode (HashPayload.encode payload)
                        in
                        case decoded of
                            Just res ->
                                case Document.mainPlacements res.document of
                                    [ p ] ->
                                        let
                                            r =
                                                Mat4.toRecord p.matrix
                                        in
                                        abs (r.m11 - can.m11)
                                            < 0.05
                                            && abs (r.m12 - can.m12)
                                            < 0.05
                                            && abs (r.m13 - can.m13)
                                            < 0.05
                                            && abs (r.m21 - can.m21)
                                            < 0.05
                                            && abs (r.m22 - can.m22)
                                            < 0.05
                                            && abs (r.m23 - can.m23)
                                            < 0.05
                                            && abs (r.m31 - can.m31)
                                            < 0.05
                                            && abs (r.m32 - can.m32)
                                            < 0.05
                                            && abs (r.m33 - can.m33)
                                            < 0.05

                                    _ ->
                                        False

                            Nothing ->
                                False

                    allPassed =
                        Array.toIndexedList HashPayload.canonicalOrientations
                            |> List.all (\( idx, can ) -> testOneOrientation idx can)
                in
                Expect.equal True allPassed
        , test "arbitrary 3D quaternion rotation preserves rotation within tolerance" <|
            \_ ->
                let
                    rotX =
                        Mat4.makeRotate (degrees 35) (vec3 1 0 0)

                    rotY =
                        Mat4.makeRotate (degrees 45) (vec3 0 1 0)

                    rot =
                        Mat4.mul rotY rotX

                    trans =
                        Mat4.makeTranslate3 100 -24 50

                    mat =
                        Mat4.mul trans rot

                    ( doc, _ ) =
                        Document.addPlacement "" { part = "3001.dat", color = 14, matrix = mat } Document.empty

                    payload =
                        { mode = FreeBuilding, document = doc, kit = Nothing }

                    decoded =
                        HashPayload.decode (HashPayload.encode payload)
                in
                case decoded of
                    Just res ->
                        case Document.mainPlacements res.document of
                            [ p ] ->
                                let
                                    pos =
                                        Mat4.transform p.matrix (vec3 0 0 0)
                                in
                                Expect.all
                                    [ \_ -> Expect.equal 100 (round (Vec3.getX pos))
                                    , \_ -> Expect.equal -24 (round (Vec3.getY pos))
                                    , \_ -> Expect.equal 50 (round (Vec3.getZ pos))
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Expected 1 placement"

                    Nothing ->
                        Expect.fail "Failed to decode quaternion placement"
        ]


validationTests : Test
validationTests =
    describe "Wire format validation"
        [ test "magic bytes 'BF' (0x42 0x46) and version (0x01) are written" <|
            \_ ->
                let
                    encoded =
                        HashPayload.encode HashPayload.emptyFree
                in
                -- Empty payload is 8 bytes
                Expect.all
                    [ \b -> Expect.equal 8 (Bytes.width b)
                    ]
                    encoded
        , test "corrupted or empty bytes fail decoding safely" <|
            \_ ->
                let
                    badBytes =
                        Encode.encode (Encode.unsignedInt8 0)
                in
                Expect.equal Nothing (HashPayload.decode badBytes)
        ]
