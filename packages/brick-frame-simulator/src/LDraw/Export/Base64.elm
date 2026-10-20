module LDraw.Export.Base64 exposing (encodeBytes, encodeString)

{-| Pure Elm Base64 encoder for binary exports (GLB, USDZ, data URIs).
-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)


charTable : Array Char
charTable =
    Array.fromList
        [ 'A'
        , 'B'
        , 'C'
        , 'D'
        , 'E'
        , 'F'
        , 'G'
        , 'H'
        , 'I'
        , 'J'
        , 'K'
        , 'L'
        , 'M'
        , 'N'
        , 'O'
        , 'P'
        , 'Q'
        , 'R'
        , 'S'
        , 'T'
        , 'U'
        , 'V'
        , 'W'
        , 'X'
        , 'Y'
        , 'Z'
        , 'a'
        , 'b'
        , 'c'
        , 'd'
        , 'e'
        , 'f'
        , 'g'
        , 'h'
        , 'i'
        , 'j'
        , 'k'
        , 'l'
        , 'm'
        , 'n'
        , 'o'
        , 'p'
        , 'q'
        , 'r'
        , 's'
        , 't'
        , 'u'
        , 'v'
        , 'w'
        , 'x'
        , 'y'
        , 'z'
        , '0'
        , '1'
        , '2'
        , '3'
        , '4'
        , '5'
        , '6'
        , '7'
        , '8'
        , '9'
        , '+'
        , '/'
        ]


lookupChar : Int -> Char
lookupChar index =
    Array.get (Bitwise.and index 0x3F) charTable
        |> Maybe.withDefault 'A'


{-| Encode a list of 8-bit integers (bytes) to a Base64 string.
-}
encodeIntList : List Int -> String
encodeIntList bytesList =
    let
        step : List Int -> List Char -> List Char
        step remaining acc =
            case remaining of
                [] ->
                    acc

                [ b0 ] ->
                    let
                        c0 =
                            lookupChar (Bitwise.shiftRightBy 2 b0)

                        c1 =
                            lookupChar (Bitwise.shiftLeftBy 4 (Bitwise.and b0 0x03))
                    in
                    '=' :: '=' :: c1 :: c0 :: acc

                [ b0, b1 ] ->
                    let
                        c0 =
                            lookupChar (Bitwise.shiftRightBy 2 b0)

                        c1 =
                            lookupChar
                                (Bitwise.or
                                    (Bitwise.shiftLeftBy 4 (Bitwise.and b0 0x03))
                                    (Bitwise.shiftRightBy 4 b1)
                                )

                        c2 =
                            lookupChar (Bitwise.shiftLeftBy 2 (Bitwise.and b1 0x0F))
                    in
                    '=' :: c2 :: c1 :: c0 :: acc

                b0 :: b1 :: b2 :: rest ->
                    let
                        c0 =
                            lookupChar (Bitwise.shiftRightBy 2 b0)

                        c1 =
                            lookupChar
                                (Bitwise.or
                                    (Bitwise.shiftLeftBy 4 (Bitwise.and b0 0x03))
                                    (Bitwise.shiftRightBy 4 b1)
                                )

                        c2 =
                            lookupChar
                                (Bitwise.or
                                    (Bitwise.shiftLeftBy 2 (Bitwise.and b1 0x0F))
                                    (Bitwise.shiftRightBy 6 b2)
                                )

                        c3 =
                            lookupChar (Bitwise.and b2 0x3F)
                    in
                    step rest (c3 :: c2 :: c1 :: c0 :: acc)
    in
    String.fromList (List.reverse (step bytesList []))


byteListDecoder : Int -> Decoder (List Int)
byteListDecoder count =
    Decode.loop ( count, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                Decode.succeed (Decode.Done (List.reverse acc))

            else
                Decode.map (\byte -> Decode.Loop ( remaining - 1, byte :: acc )) Decode.unsignedInt8
        )


{-| Encode a raw `Bytes` payload into a Base64 string.
-}
encodeBytes : Bytes -> String
encodeBytes bytes =
    let
        len =
            Bytes.width bytes
    in
    if len <= 0 then
        ""

    else
        case Decode.decode (byteListDecoder len) bytes of
            Just list ->
                encodeIntList list

            Nothing ->
                ""


{-| Encode an ASCII/UTF-8 string into a Base64 string.
-}
encodeString : String -> String
encodeString str =
    str
        |> String.toList
        |> List.map Char.toCode
        |> encodeIntList
