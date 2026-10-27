module Studio.HashPayload exposing
    ( KitItem
    , Mat3Record
    , Mode(..)
    , Payload
    , bytesToList
    , canonicalOrientations
    , decode
    , decodeBytes
    , decodeLeb128
    , emptyFree
    , encode
    , encodeBytes
    , encodeLeb128
    , extractKit
    , fromDocument
    , kitOfPlacements
    , listToBytes
    , toDocument
    , zigZagDecode
    , zigZagEncode
    )

{-| High-density binary hash link encoder and decoder for Brick Play and companion applications.

Implements the wire format specification:

  - Magic bytes: 0x42 0x46 ("BF"), version 0x01
  - Header flags:
      - Bit 0: Mode (0 = Free building, 1 = Puzzle mode)
      - Bit 1: Kit present (0 = None, 1 = Kit inventory block present)
      - Bit 2: Glued placements present (0 = None, 1 = Glued placements block present)
  - Dictionaries:
      - Part Dictionary: U8 count of unique parts, followed by each part name (U8 length + ASCII chars, stripped of ".dat")
      - Color Dictionary: U8 count of unique colors, followed by each U16LE color code
  - Placements:
      - If glued flag: U16LE glued\_count, followed by glued placement records
      - Followed by U16LE loose\_count, followed by loose placement records
      - Placement record:
          - part\_idx (U8)
          - color\_idx (U8)
          - transform\_flags / rotation (U8):
              - Mode 00 (Discrete Orthogonal): Bits 0..4 = 0..23 canonical orientation index
              - Mode 01 (Orthogonal + Yaw): Bits 0..4 = 0..23 base orientation, followed by U16LE yaw angle
              - Mode 10 (Full 3D / Quaternion): 3 x I16LE components
          - Delta position: delta\_x, delta\_y, delta\_z (ZigZag encoded LEB128 varints in 1 LDU integers)
  - Kit Inventory Block (if puzzle mode and kit present):
      - U8 kit\_item\_count, followed by entries: part\_idx (U8), color\_idx (U8), count (U8)

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode exposing (Encoder)
import Dict exposing (Dict)
import LDraw.Document as Document exposing (Document)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set exposing (Set)


{-| Application mode stored in the binary payload header.
-}
type Mode
    = FreeBuilding
    | Puzzle


{-| A kit inventory item specifying part file name, LDraw colour code, and count.
-}
type alias KitItem =
    { part : String
    , color : Int
    , count : Int
    }


{-| Decoded payload from binary data.
-}
type alias Payload =
    { mode : Mode
    , document : Document
    , kit : Maybe (List KitItem)
    }


{-| An unrotated, empty board in free building mode.
-}
emptyFree : Payload
emptyFree =
    { mode = FreeBuilding
    , document = Document.empty
    , kit = Nothing
    }


{-| A 3×3 rotation matrix representation with row-major named entries.
-}
type alias Mat3Record =
    { m11 : Float
    , m12 : Float
    , m13 : Float
    , m21 : Float
    , m22 : Float
    , m23 : Float
    , m31 : Float
    , m32 : Float
    , m33 : Float
    }


{-| The 24 canonical proper orthogonal orientations (the octahedral rotation group O).
Orientation 0 is the identity matrix.
-}
canonicalOrientations : Array Mat3Record
canonicalOrientations =
    Array.fromList
        [ { m11 = 1, m12 = 0, m13 = 0, m21 = 0, m22 = 1, m23 = 0, m31 = 0, m32 = 0, m33 = 1 }
        , { m11 = -1, m12 = 0, m13 = 0, m21 = 0, m22 = 1, m23 = 0, m31 = 0, m32 = 0, m33 = -1 }
        , { m11 = 0, m12 = 0, m13 = -1, m21 = 0, m22 = 1, m23 = 0, m31 = 1, m32 = 0, m33 = 0 }
        , { m11 = 0, m12 = 0, m13 = 1, m21 = 0, m22 = 1, m23 = 0, m31 = -1, m32 = 0, m33 = 0 }
        , { m11 = 1, m12 = 0, m13 = 0, m21 = 0, m22 = -1, m23 = 0, m31 = 0, m32 = 0, m33 = -1 }
        , { m11 = -1, m12 = 0, m13 = 0, m21 = 0, m22 = -1, m23 = 0, m31 = 0, m32 = 0, m33 = 1 }
        , { m11 = 0, m12 = 0, m13 = 1, m21 = 0, m22 = -1, m23 = 0, m31 = 1, m32 = 0, m33 = 0 }
        , { m11 = 0, m12 = 0, m13 = -1, m21 = 0, m22 = -1, m23 = 0, m31 = -1, m32 = 0, m33 = 0 }
        , { m11 = 0, m12 = 1, m13 = 0, m21 = 1, m22 = 0, m23 = 0, m31 = 0, m32 = 0, m33 = -1 }
        , { m11 = 0, m12 = 1, m13 = 0, m21 = -1, m22 = 0, m23 = 0, m31 = 0, m32 = 0, m33 = 1 }
        , { m11 = 0, m12 = 1, m13 = 0, m21 = 0, m22 = 0, m23 = 1, m31 = 1, m32 = 0, m33 = 0 }
        , { m11 = 0, m12 = 1, m13 = 0, m21 = 0, m22 = 0, m23 = -1, m31 = -1, m32 = 0, m33 = 0 }
        , { m11 = 0, m12 = -1, m13 = 0, m21 = 1, m22 = 0, m23 = 0, m31 = 0, m32 = 0, m33 = 1 }
        , { m11 = 0, m12 = -1, m13 = 0, m21 = -1, m22 = 0, m23 = 0, m31 = 0, m32 = 0, m33 = -1 }
        , { m11 = 0, m12 = -1, m13 = 0, m21 = 0, m22 = 0, m23 = -1, m31 = 1, m32 = 0, m33 = 0 }
        , { m11 = 0, m12 = -1, m13 = 0, m21 = 0, m22 = 0, m23 = 1, m31 = -1, m32 = 0, m33 = 0 }
        , { m11 = 1, m12 = 0, m13 = 0, m21 = 0, m22 = 0, m23 = -1, m31 = 0, m32 = 1, m33 = 0 }
        , { m11 = -1, m12 = 0, m13 = 0, m21 = 0, m22 = 0, m23 = 1, m31 = 0, m32 = 1, m33 = 0 }
        , { m11 = 0, m12 = 0, m13 = 1, m21 = 1, m22 = 0, m23 = 0, m31 = 0, m32 = 1, m33 = 0 }
        , { m11 = 0, m12 = 0, m13 = -1, m21 = -1, m22 = 0, m23 = 0, m31 = 0, m32 = 1, m33 = 0 }
        , { m11 = 1, m12 = 0, m13 = 0, m21 = 0, m22 = 0, m23 = 1, m31 = 0, m32 = -1, m33 = 0 }
        , { m11 = -1, m12 = 0, m13 = 0, m21 = 0, m22 = 0, m23 = -1, m31 = 0, m32 = -1, m33 = 0 }
        , { m11 = 0, m12 = 0, m13 = -1, m21 = 1, m22 = 0, m23 = 0, m31 = 0, m32 = -1, m33 = 0 }
        , { m11 = 0, m12 = 0, m13 = 1, m21 = -1, m22 = 0, m23 = 0, m31 = 0, m32 = -1, m33 = 0 }
        ]



-- ── ZigZag & LEB128 Varints ───────────────────────────────────────────────────


{-| ZigZag encode a signed integer to an unsigned integer.
-}
zigZagEncode : Int -> Int
zigZagEncode n =
    if n >= 0 then
        2 * n

    else
        -2 * n - 1


{-| ZigZag decode an unsigned integer to a signed integer.
-}
zigZagDecode : Int -> Int
zigZagDecode u =
    if modBy 2 u == 0 then
        u // 2

    else
        -(u + 1) // 2


{-| Encode an unsigned integer as a list of LEB128 bytes.
-}
encodeLeb128 : Int -> List Int
encodeLeb128 v =
    if v < 128 then
        [ v ]

    else
        let
            byte =
                Bitwise.or (Bitwise.and v 0x7F) 0x80

            rest =
                v // 128
        in
        byte :: encodeLeb128 rest


encodeVarint : Int -> Encoder
encodeVarint n =
    Encode.sequence (List.map Encode.unsignedInt8 (encodeLeb128 (zigZagEncode n)))


{-| Decode an unsigned LEB128 varint.
-}
decodeLeb128 : Decoder Int
decodeLeb128 =
    Decode.loop ( 0, 0 )
        (\( acc, shift ) ->
            Decode.unsignedInt8
                |> Decode.map
                    (\b ->
                        let
                            val =
                                Bitwise.and b 0x7F

                            newAcc =
                                acc + Bitwise.shiftLeftBy shift val
                        in
                        if Bitwise.and b 0x80 == 0 then
                            Decode.Done newAcc

                        else
                            Decode.Loop ( newAcc, shift + 7 )
                    )
        )


decodeVarint : Decoder Int
decodeVarint =
    Decode.map zigZagDecode decodeLeb128



-- ── Helpers ───────────────────────────────────────────────────────────────────


{-| Convert Bytes into a list of unsigned 8-bit integers.
-}
bytesToList : Bytes -> List Int
bytesToList bytes =
    Decode.decode
        (Decode.loop ( Bytes.width bytes, [] )
            (\( remaining, acc ) ->
                if remaining <= 0 then
                    Decode.succeed (Decode.Done (List.reverse acc))

                else
                    Decode.unsignedInt8 |> Decode.map (\b -> Decode.Loop ( remaining - 1, b :: acc ))
            )
        )
        bytes
        |> Maybe.withDefault []


{-| Convert a list of unsigned 8-bit integers into Bytes.
-}
listToBytes : List Int -> Bytes
listToBytes list =
    Encode.encode (Encode.sequence (List.map Encode.unsignedInt8 list))


{-| Encode a Payload into a list of bytes (suitable for ports and flags).
-}
encodeBytes : Payload -> List Int
encodeBytes payload =
    bytesToList (encode payload)


{-| Decode a list of bytes (from ports or flags) into a Payload.
-}
decodeBytes : List Int -> Maybe Payload
decodeBytes list =
    decode (listToBytes list)


stripDat : String -> String
stripDat name =
    let
        lower =
            String.toLower name
    in
    if String.endsWith ".dat" lower then
        String.dropRight 4 name

    else
        name


ensureDat : String -> String
ensureDat name =
    let
        lower =
            String.toLower name
    in
    if String.endsWith ".dat" lower then
        name

    else
        name ++ ".dat"


extractPlacements : Document -> ( List Document.Placement, List Document.Placement )
extractPlacements doc =
    let
        gluedSet =
            Document.gluedIds doc
    in
    Document.mainPlacements doc
        |> List.partition (\p -> Set.member p.id gluedSet)


{-| Group a list of placements into a kit inventory.
-}
kitOfPlacements : List Document.Placement -> List KitItem
kitOfPlacements placements =
    let
        counts =
            List.foldl
                (\p acc ->
                    let
                        key =
                            p.part ++ ":" ++ String.fromInt p.color
                    in
                    Dict.update key
                        (\existing ->
                            case existing of
                                Just item ->
                                    Just { item | count = item.count + 1 }

                                Nothing ->
                                    Just { part = p.part, color = p.color, count = 1 }
                        )
                        acc
                )
                Dict.empty
                placements
    in
    counts
        |> Dict.values
        |> List.sortWith
            (\a b ->
                case compare a.part b.part of
                    EQ ->
                        compare a.color b.color

                    other ->
                        other
            )


{-| Group document main placements into a kit inventory.
-}
extractKit : Document -> List KitItem
extractKit doc =
    kitOfPlacements (Document.mainPlacements doc)


{-| Reconstruct a Document from glued and loose placements.
-}
toDocument : List { part : String, color : Int, matrix : Mat4 } -> List { part : String, color : Int, matrix : Mat4 } -> Document
toDocument glued loose =
    let
        addOne spec isGlued acc =
            let
                ( nextDoc, newId ) =
                    Document.addPlacement "" spec acc
            in
            if isGlued then
                Document.setGlued True newId nextDoc

            else
                nextDoc

        withGlued =
            List.foldl (\spec acc -> addOne spec True acc) Document.empty glued
    in
    List.foldl (\spec acc -> addOne spec False acc) withGlued loose


{-| Split a document into its glued and loose placements.
-}
fromDocument : Document -> ( List Document.Placement, List Document.Placement )
fromDocument =
    extractPlacements



-- ── Matrix & Rotation Codec ───────────────────────────────────────────────────


approxEqual : Float -> Float -> Bool
approxEqual a b =
    abs (a - b) < 0.01


matchesCanonical : Mat4 -> Mat3Record -> Bool
matchesCanonical mat can =
    let
        r =
            Mat4.toRecord mat
    in
    approxEqual r.m11 can.m11
        && approxEqual r.m12 can.m12
        && approxEqual r.m13 can.m13
        && approxEqual r.m21 can.m21
        && approxEqual r.m22 can.m22
        && approxEqual r.m23 can.m23
        && approxEqual r.m31 can.m31
        && approxEqual r.m32 can.m32
        && approxEqual r.m33 can.m33


findCanonical : Mat4 -> Maybe Int
findCanonical mat =
    let
        check i =
            if i >= Array.length canonicalOrientations then
                Nothing

            else
                case Array.get i canonicalOrientations of
                    Just can ->
                        if matchesCanonical mat can then
                            Just i

                        else
                            check (i + 1)

                    Nothing ->
                        Nothing
    in
    check 0


type RotationEncoding
    = Orthogonal Int
    | OrthogonalYaw Int Int
    | Quaternion Int Int Int


type alias Quat =
    { qx : Float
    , qy : Float
    , qz : Float
    , qw : Float
    }


matToQuat : Mat4 -> Quat
matToQuat mat =
    let
        r =
            Mat4.toRecord mat

        tr =
            r.m11 + r.m22 + r.m33
    in
    if tr > 0 then
        let
            s =
                2 * sqrt (tr + 1)

            qw =
                0.25 * s

            qx =
                (r.m32 - r.m23) / s

            qy =
                (r.m13 - r.m31) / s

            qz =
                (r.m21 - r.m12) / s
        in
        normalizeQuat { qx = qx, qy = qy, qz = qz, qw = qw }

    else if r.m11 > r.m22 && r.m11 > r.m33 then
        let
            s =
                2 * sqrt (1 + r.m11 - r.m22 - r.m33)

            qw =
                (r.m32 - r.m23) / s

            qx =
                0.25 * s

            qy =
                (r.m12 + r.m21) / s

            qz =
                (r.m13 + r.m31) / s
        in
        normalizeQuat { qx = qx, qy = qy, qz = qz, qw = qw }

    else if r.m22 > r.m33 then
        let
            s =
                2 * sqrt (1 + r.m22 - r.m11 - r.m33)

            qw =
                (r.m13 - r.m31) / s

            qx =
                (r.m12 + r.m21) / s

            qy =
                0.25 * s

            qz =
                (r.m23 + r.m32) / s
        in
        normalizeQuat { qx = qx, qy = qy, qz = qz, qw = qw }

    else
        let
            s =
                2 * sqrt (1 + r.m33 - r.m11 - r.m22)

            qw =
                (r.m21 - r.m12) / s

            qx =
                (r.m13 + r.m31) / s

            qy =
                (r.m23 + r.m32) / s

            qz =
                0.25 * s
        in
        normalizeQuat { qx = qx, qy = qy, qz = qz, qw = qw }


normalizeQuat : Quat -> Quat
normalizeQuat q =
    let
        len =
            sqrt (q.qx * q.qx + q.qy * q.qy + q.qz * q.qz + q.qw * q.qw)

        nq =
            if len > 0 then
                { qx = q.qx / len, qy = q.qy / len, qz = q.qz / len, qw = q.qw / len }

            else
                { qx = 0, qy = 0, qz = 0, qw = 1 }
    in
    if nq.qw < 0 then
        { qx = -nq.qx, qy = -nq.qy, qz = -nq.qz, qw = -nq.qw }

    else
        nq


encodeRotation : Mat4 -> RotationEncoding
encodeRotation mat =
    case findCanonical mat of
        Just idx ->
            Orthogonal idx

        Nothing ->
            let
                q =
                    matToQuat mat

                ix =
                    round (clamp -1.0 1.0 q.qx * 32767.0)

                iy =
                    round (clamp -1.0 1.0 q.qy * 32767.0)

                iz =
                    round (clamp -1.0 1.0 q.qz * 32767.0)
            in
            Quaternion ix iy iz


rotationEncoder : RotationEncoding -> List Encoder
rotationEncoder rot =
    case rot of
        Orthogonal idx ->
            [ Encode.unsignedInt8 (Bitwise.and idx 0x1F) ]

        OrthogonalYaw baseIdx yawU16 ->
            [ Encode.unsignedInt8 (Bitwise.or 0x40 (Bitwise.and baseIdx 0x1F))
            , Encode.unsignedInt16 Bytes.LE yawU16
            ]

        Quaternion ix iy iz ->
            [ Encode.unsignedInt8 0x80
            , Encode.signedInt16 Bytes.LE ix
            , Encode.signedInt16 Bytes.LE iy
            , Encode.signedInt16 Bytes.LE iz
            ]


decodeRotation : Decoder Mat3Record
decodeRotation =
    Decode.unsignedInt8
        |> Decode.andThen
            (\flagByte ->
                let
                    mode =
                        Bitwise.shiftRightBy 6 flagByte

                    baseIdx =
                        Bitwise.and flagByte 0x1F
                in
                case mode of
                    0 ->
                        -- Discrete Orthogonal
                        case Array.get baseIdx canonicalOrientations of
                            Just record ->
                                Decode.succeed record

                            Nothing ->
                                Decode.succeed { m11 = 1, m12 = 0, m13 = 0, m21 = 0, m22 = 1, m23 = 0, m31 = 0, m32 = 0, m33 = 1 }

                    1 ->
                        -- Orthogonal + Yaw
                        Decode.unsignedInt16 Bytes.LE
                            |> Decode.map
                                (\yawU16 ->
                                    let
                                        base =
                                            Array.get baseIdx canonicalOrientations
                                                |> Maybe.withDefault { m11 = 1, m12 = 0, m13 = 0, m21 = 0, m22 = 1, m23 = 0, m31 = 0, m32 = 0, m33 = 1 }

                                        rad =
                                            (toFloat yawU16 / 100.0) * pi / 180.0

                                        cosA =
                                            cos rad

                                        sinA =
                                            sin rad

                                        -- Multiply base * Ry(rad)
                                        m11 =
                                            base.m11 * cosA - base.m13 * sinA

                                        m12 =
                                            base.m12

                                        m13 =
                                            base.m11 * sinA + base.m13 * cosA

                                        m21 =
                                            base.m21 * cosA - base.m23 * sinA

                                        m22 =
                                            base.m22

                                        m23 =
                                            base.m21 * sinA + base.m23 * cosA

                                        m31 =
                                            base.m31 * cosA - base.m33 * sinA

                                        m32 =
                                            base.m32

                                        m33 =
                                            base.m31 * sinA + base.m33 * cosA
                                    in
                                    { m11 = m11, m12 = m12, m13 = m13, m21 = m21, m22 = m22, m23 = m23, m31 = m31, m32 = m32, m33 = m33 }
                                )

                    _ ->
                        -- Full 3D / Quaternion
                        Decode.map3
                            (\ix iy iz ->
                                let
                                    qx =
                                        toFloat ix / 32767.0

                                    qy =
                                        toFloat iy / 32767.0

                                    qz =
                                        toFloat iz / 32767.0

                                    qw =
                                        sqrt (max 0.0 (1.0 - (qx * qx + qy * qy + qz * qz)))

                                    m11 =
                                        1 - 2 * (qy * qy + qz * qz)

                                    m12 =
                                        2 * (qx * qy - qz * qw)

                                    m13 =
                                        2 * (qx * qz + qy * qw)

                                    m21 =
                                        2 * (qx * qy + qz * qw)

                                    m22 =
                                        1 - 2 * (qx * qx + qz * qz)

                                    m23 =
                                        2 * (qy * qz - qx * qw)

                                    m31 =
                                        2 * (qx * qz - qy * qw)

                                    m32 =
                                        2 * (qy * qz + qx * qw)

                                    m33 =
                                        1 - 2 * (qx * qx + qy * qy)
                                in
                                { m11 = m11, m12 = m12, m13 = m13, m21 = m21, m22 = m22, m23 = m23, m31 = m31, m32 = m32, m33 = m33 }
                            )
                            (Decode.signedInt16 Bytes.LE)
                            (Decode.signedInt16 Bytes.LE)
                            (Decode.signedInt16 Bytes.LE)
            )


makeMat4 : Mat3Record -> ( Float, Float, Float ) -> Mat4
makeMat4 r ( x, y, z ) =
    Mat4.fromRecord
        { m11 = r.m11
        , m12 = r.m12
        , m13 = r.m13
        , m14 = x
        , m21 = r.m21
        , m22 = r.m22
        , m23 = r.m23
        , m24 = y
        , m31 = r.m31
        , m32 = r.m32
        , m33 = r.m33
        , m34 = z
        , m41 = 0
        , m42 = 0
        , m43 = 0
        , m44 = 1
        }



-- ── Main Encoders ─────────────────────────────────────────────────────────────


{-| Encode a Payload into wire-format Bytes.
-}
encode : Payload -> Bytes
encode payload =
    let
        ( glued, loose ) =
            extractPlacements payload.document

        kitItems =
            case payload.mode of
                Puzzle ->
                    payload.kit |> Maybe.withDefault []

                FreeBuilding ->
                    []

        -- Collect unique parts
        allParts =
            List.map .part glued
                ++ List.map .part loose
                ++ List.map .part kitItems

        uniqueParts =
            allParts
                |> List.foldl
                    (\p acc ->
                        if List.member p acc then
                            acc

                        else
                            acc ++ [ p ]
                    )
                    []

        partDict =
            List.indexedMap (\i p -> ( p, i )) uniqueParts
                |> Dict.fromList

        -- Collect unique colors
        allColors =
            List.map .color glued
                ++ List.map .color loose
                ++ List.map .color kitItems

        uniqueColors =
            allColors
                |> List.foldl
                    (\c acc ->
                        if List.member c acc then
                            acc

                        else
                            acc ++ [ c ]
                    )
                    []

        colorDict =
            List.indexedMap (\i c -> ( c, i )) uniqueColors
                |> Dict.fromList

        -- Header flags
        modeBit =
            case payload.mode of
                FreeBuilding ->
                    0

                Puzzle ->
                    1

        kitBit =
            case ( payload.mode, payload.kit ) of
                ( Puzzle, Just _ ) ->
                    2

                _ ->
                    0

        gluedBit =
            if List.isEmpty glued then
                0

            else
                4

        headerFlags =
            Bitwise.or modeBit (Bitwise.or kitBit gluedBit)

        -- Part dictionary encoder
        partDictEncoder =
            Encode.sequence
                (Encode.unsignedInt8 (List.length uniqueParts)
                    :: List.map
                        (\partName ->
                            let
                                stripped =
                                    stripDat partName

                                len =
                                    Encode.getStringWidth stripped
                            in
                            Encode.sequence
                                [ Encode.unsignedInt8 len
                                , Encode.string stripped
                                ]
                        )
                        uniqueParts
                )

        -- Color dictionary encoder
        colorDictEncoder =
            Encode.sequence
                (Encode.unsignedInt8 (List.length uniqueColors)
                    :: List.map (\colorCode -> Encode.unsignedInt16 Bytes.LE colorCode) uniqueColors
                )

        -- Placements block encoder
        encodePlacementsList list =
            let
                step p state =
                    let
                        pos =
                            Mat4.transform p.matrix (vec3 0 0 0)

                        curX =
                            round (Vec3.getX pos)

                        curY =
                            round (Vec3.getY pos)

                        curZ =
                            round (Vec3.getZ pos)

                        deltaX =
                            curX - state.prevX

                        deltaY =
                            curY - state.prevY

                        deltaZ =
                            curZ - state.prevZ

                        partIdx =
                            Dict.get p.part partDict |> Maybe.withDefault 0

                        colorIdx =
                            Dict.get p.color colorDict |> Maybe.withDefault 0

                        rotEncoders =
                            rotationEncoder (encodeRotation p.matrix)

                        recordEncoder =
                            Encode.sequence
                                ([ Encode.unsignedInt8 partIdx
                                 , Encode.unsignedInt8 colorIdx
                                 ]
                                    ++ rotEncoders
                                    ++ [ encodeVarint deltaX
                                       , encodeVarint deltaY
                                       , encodeVarint deltaZ
                                       ]
                                )
                    in
                    { prevX = curX
                    , prevY = curY
                    , prevZ = curZ
                    , encoders = recordEncoder :: state.encoders
                    }

                finalState =
                    List.foldl step { prevX = 0, prevY = 0, prevZ = 0, encoders = [] } list
            in
            List.reverse finalState.encoders

        gluedSection =
            if gluedBit /= 0 then
                [ Encode.unsignedInt16 Bytes.LE (List.length glued)
                , Encode.sequence (encodePlacementsList glued)
                ]

            else
                []

        looseSection =
            [ Encode.unsignedInt16 Bytes.LE (List.length loose)
            , Encode.sequence (encodePlacementsList loose)
            ]

        -- Kit inventory block encoder
        kitSection =
            case ( payload.mode, payload.kit ) of
                ( Puzzle, Just items ) ->
                    [ Encode.unsignedInt8 (List.length items)
                    , Encode.sequence
                        (List.map
                            (\item ->
                                let
                                    pIdx =
                                        Dict.get item.part partDict |> Maybe.withDefault 0

                                    cIdx =
                                        Dict.get item.color colorDict |> Maybe.withDefault 0
                                in
                                Encode.sequence
                                    [ Encode.unsignedInt8 pIdx
                                    , Encode.unsignedInt8 cIdx
                                    , Encode.unsignedInt8 (clamp 1 255 item.count)
                                    ]
                            )
                            items
                        )
                    ]

                _ ->
                    []

        fullEncoder =
            Encode.sequence
                ([ Encode.unsignedInt8 0x42
                 , Encode.unsignedInt8 0x46
                 , Encode.unsignedInt8 0x01
                 , Encode.unsignedInt8 headerFlags
                 , partDictEncoder
                 , colorDictEncoder
                 ]
                    ++ gluedSection
                    ++ looseSection
                    ++ kitSection
                )
    in
    Encode.encode fullEncoder



-- ── Main Decoders ─────────────────────────────────────────────────────────────


{-| Decode wire-format Bytes into a Payload.
-}
decode : Bytes -> Maybe Payload
decode bytes =
    Decode.decode payloadDecoder bytes


payloadDecoder : Decoder Payload
payloadDecoder =
    Decode.unsignedInt8
        |> Decode.andThen
            (\m1 ->
                Decode.unsignedInt8
                    |> Decode.andThen
                        (\m2 ->
                            if m1 /= 0x42 || m2 /= 0x46 then
                                Decode.fail

                            else
                                Decode.unsignedInt8
                                    |> Decode.andThen
                                        (\version ->
                                            if version /= 0x01 then
                                                Decode.fail

                                            else
                                                Decode.unsignedInt8
                                                    |> Decode.andThen decodeWithFlags
                                        )
                        )
            )


decodeWithFlags : Int -> Decoder Payload
decodeWithFlags flags =
    let
        mode =
            if Bitwise.and flags 0x01 /= 0 then
                Puzzle

            else
                FreeBuilding

        hasKit =
            Bitwise.and flags 0x02 /= 0

        hasGlued =
            Bitwise.and flags 0x04 /= 0
    in
    decodePartDict
        |> Decode.andThen
            (\partDict ->
                decodeColorDict
                    |> Decode.andThen
                        (\colorDict ->
                            decodeGluedSection hasGlued partDict colorDict
                                |> Decode.andThen
                                    (\gluedPlacements ->
                                        decodeLooseSection partDict colorDict
                                            |> Decode.andThen
                                                (\loosePlacements ->
                                                    decodeKitSection mode hasKit partDict colorDict
                                                        |> Decode.map
                                                            (\kit ->
                                                                { mode = mode
                                                                , document = toDocument gluedPlacements loosePlacements
                                                                , kit = kit
                                                                }
                                                            )
                                                )
                                    )
                        )
            )


decodePartDict : Decoder (Array String)
decodePartDict =
    Decode.unsignedInt8
        |> Decode.andThen
            (\count ->
                Decode.loop ( count, [] )
                    (\( remaining, acc ) ->
                        if remaining <= 0 then
                            Decode.succeed (Decode.Done (Array.fromList (List.reverse acc)))

                        else
                            Decode.unsignedInt8
                                |> Decode.andThen
                                    (\len ->
                                        Decode.string len
                                            |> Decode.map
                                                (\str ->
                                                    Decode.Loop ( remaining - 1, ensureDat str :: acc )
                                                )
                                    )
                    )
            )


decodeColorDict : Decoder (Array Int)
decodeColorDict =
    Decode.unsignedInt8
        |> Decode.andThen
            (\count ->
                Decode.loop ( count, [] )
                    (\( remaining, acc ) ->
                        if remaining <= 0 then
                            Decode.succeed (Decode.Done (Array.fromList (List.reverse acc)))

                        else
                            Decode.unsignedInt16 Bytes.LE
                                |> Decode.map (\c -> Decode.Loop ( remaining - 1, c :: acc ))
                    )
            )


type alias DecodedPlacement =
    { part : String
    , color : Int
    , matrix : Mat4
    }


decodePlacementList : Int -> Array String -> Array Int -> Decoder (List DecodedPlacement)
decodePlacementList totalCount partDict colorDict =
    Decode.loop { remaining = totalCount, prevX = 0, prevY = 0, prevZ = 0, acc = [] }
        (\state ->
            if state.remaining <= 0 then
                Decode.succeed (Decode.Done (List.reverse state.acc))

            else
                Decode.map2 Tuple.pair Decode.unsignedInt8 Decode.unsignedInt8
                    |> Decode.andThen
                        (\( pIdx, cIdx ) ->
                            decodeRotation
                                |> Decode.andThen
                                    (\rotRecord ->
                                        Decode.map3 (\dx dy dz -> ( dx, dy, dz ))
                                            decodeVarint
                                            decodeVarint
                                            decodeVarint
                                            |> Decode.map
                                                (\( deltaX, deltaY, deltaZ ) ->
                                                    let
                                                        curX =
                                                            state.prevX + deltaX

                                                        curY =
                                                            state.prevY + deltaY

                                                        curZ =
                                                            state.prevZ + deltaZ

                                                        partName =
                                                            Array.get pIdx partDict |> Maybe.withDefault "3001.dat"

                                                        colorCode =
                                                            Array.get cIdx colorDict |> Maybe.withDefault 16

                                                        mat =
                                                            makeMat4 rotRecord ( toFloat curX, toFloat curY, toFloat curZ )

                                                        placement =
                                                            { part = partName
                                                            , color = colorCode
                                                            , matrix = mat
                                                            }
                                                    in
                                                    Decode.Loop
                                                        { remaining = state.remaining - 1
                                                        , prevX = curX
                                                        , prevY = curY
                                                        , prevZ = curZ
                                                        , acc = placement :: state.acc
                                                        }
                                                )
                                    )
                        )
        )


decodeGluedSection : Bool -> Array String -> Array Int -> Decoder (List DecodedPlacement)
decodeGluedSection hasGlued partDict colorDict =
    if hasGlued then
        Decode.unsignedInt16 Bytes.LE
            |> Decode.andThen (\count -> decodePlacementList count partDict colorDict)

    else
        Decode.succeed []


decodeLooseSection : Array String -> Array Int -> Decoder (List DecodedPlacement)
decodeLooseSection partDict colorDict =
    Decode.unsignedInt16 Bytes.LE
        |> Decode.andThen (\count -> decodePlacementList count partDict colorDict)


decodeKitSection : Mode -> Bool -> Array String -> Array Int -> Decoder (Maybe (List KitItem))
decodeKitSection mode hasKit partDict colorDict =
    if mode == Puzzle && hasKit then
        Decode.unsignedInt8
            |> Decode.andThen
                (\count ->
                    Decode.loop ( count, [] )
                        (\( remaining, acc ) ->
                            if remaining <= 0 then
                                Decode.succeed (Decode.Done (Just (List.reverse acc)))

                            else
                                Decode.map3
                                    (\pIdx cIdx itemCnt ->
                                        let
                                            part =
                                                Array.get pIdx partDict |> Maybe.withDefault "3001.dat"

                                            color =
                                                Array.get cIdx colorDict |> Maybe.withDefault 16
                                        in
                                        Decode.Loop ( remaining - 1, { part = part, color = color, count = itemCnt } :: acc )
                                    )
                                    Decode.unsignedInt8
                                    Decode.unsignedInt8
                                    Decode.unsignedInt8
                        )
                )

    else
        Decode.succeed Nothing
