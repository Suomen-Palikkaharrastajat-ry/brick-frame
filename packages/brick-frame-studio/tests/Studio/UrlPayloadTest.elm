module Studio.UrlPayloadTest exposing (suite)

{-| Tests for Studio.UrlPayload.
-}

import Expect
import LDraw.Document as Document
import Math.Matrix4 as Mat4
import Studio.UrlPayload as UrlPayload exposing (PartKitItem)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Studio.UrlPayload"
        [ describe "extractKit"
            [ test "extracts empty kit from empty document" <|
                \_ ->
                    UrlPayload.extractKit Document.empty
                        |> Expect.equal []
            , test "groups placements by part and color and counts them" <|
                \_ ->
                    let
                        doc =
                            Document.parse
                                """1 14 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat
1 14 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat
1 4 0 0 0 1 0 0 0 1 0 0 0 1 3002.dat
1 1 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"""
                    in
                    UrlPayload.extractKit doc
                        |> Expect.equal
                            [ { part = "3001.dat", color = 1, count = 1 }
                            , { part = "3001.dat", color = 14, count = 2 }
                            , { part = "3002.dat", color = 4, count = 1 }
                            ]
            ]
        , describe "encodeParts and decodeParts"
            [ test "roundtrips part kit items" <|
                \_ ->
                    let
                        items =
                            [ { part = "3001.dat", color = 14, count = 5 }
                            , { part = "3002.dat", color = 4, count = 2 }
                            ]

                        encoded =
                            UrlPayload.encodeParts items
                    in
                    Expect.all
                        [ \_ -> Expect.equal "3001.dat:14:5,3002.dat:4:2" encoded
                        , \_ -> Expect.equal items (UrlPayload.decodeParts encoded)
                        ]
                        ()
            , test "handles empty string" <|
                \_ ->
                    UrlPayload.decodeParts ""
                        |> Expect.equal []
            , test "ignores malformed entries" <|
                \_ ->
                    UrlPayload.decodeParts "invalid,3001.dat:14:5,broken:2,3002.dat:bad:1"
                        |> Expect.equal [ { part = "3001.dat", color = 14, count = 5 } ]
            ]
        , describe "encodePlayHash and decodeHash"
            [ test "encodes and decodes play hash" <|
                \_ ->
                    let
                        doc =
                            Document.parse "1 14 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"

                        hash =
                            UrlPayload.encodePlayHash doc

                        decoded =
                            UrlPayload.decodeHash ("#" ++ hash)
                    in
                    Expect.all
                        [ \p -> Expect.equal p.parts [ { part = "3001.dat", color = 14, count = 1 } ]
                        , \p -> Expect.equal p.modelText Nothing
                        ]
                        decoded
            ]
        , describe "encodeContraptionsHash and decodeHash"
            [ test "encodes and decodes contraptions hash with model and parts" <|
                \_ ->
                    let
                        doc =
                            Document.parse "1 7 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat"

                        hash =
                            UrlPayload.encodeContraptionsHash doc

                        decoded =
                            UrlPayload.decodeHash hash
                    in
                    Expect.all
                        [ \p -> Expect.equal p.parts [ { part = "3647.dat", color = 7, count = 1 } ]
                        , \p ->
                            case p.modelText of
                                Just text ->
                                    Expect.equal (String.contains "3647.dat" text) True

                                Nothing ->
                                    Expect.fail "Expected modelText to be present"
                        ]
                        decoded
            ]
        , describe "buildPlayUrl and buildContraptionsUrl"
            [ test "builds play URL" <|
                \_ ->
                    let
                        doc =
                            Document.parse "1 14 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"

                        url =
                            UrlPayload.buildPlayUrl "http://localhost:5175/" doc
                    in
                    Expect.equal (String.startsWith "http://localhost:5175/#parts=" url) True
            , test "builds contraptions URL" <|
                \_ ->
                    let
                        doc =
                            Document.parse "1 7 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat"

                        url =
                            UrlPayload.buildContraptionsUrl "http://localhost:5176/" doc
                    in
                    Expect.equal (String.startsWith "http://localhost:5176/#model=" url) True
            ]
        ]
