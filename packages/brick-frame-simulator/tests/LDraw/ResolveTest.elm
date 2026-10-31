module LDraw.ResolveTest exposing (suite)

{-| Unit tests for LDraw part resolution, candidate ordering, and cache updates.
-}

import Expect
import LDraw.Resolve as Resolve exposing (ResolverConfig, defaultResolverConfig, partCandidateUrls)
import Test exposing (Test, describe, test)


testConfig : ResolverConfig
testConfig =
    { primaryBase = "/ldraw"
    , fallbackBase = Just "https://example.com/ldraw"
    }


suite : Test
suite =
    describe "LDraw.Resolve"
        [ describe "partCandidateUrls"
            [ test "standard numeric parts prioritize parts/ before p/" <|
                \_ ->
                    partCandidateUrls testConfig "3001.dat"
                        |> List.head
                        |> Expect.equal (Just "/ldraw/parts/3001.dat")
            , test "technic gear parts prioritize parts/ before p/" <|
                \_ ->
                    partCandidateUrls testConfig "3647.dat"
                        |> List.head
                        |> Expect.equal (Just "/ldraw/parts/3647.dat")
            , test "stud primitive prioritizes p/ before parts/" <|
                \_ ->
                    partCandidateUrls testConfig "stud.dat"
                        |> List.head
                        |> Expect.equal (Just "/ldraw/p/stud.dat")
            , test "fractional cylinder primitive prioritizes p/ before parts/" <|
                \_ ->
                    partCandidateUrls testConfig "1-4cyli.dat"
                        |> List.head
                        |> Expect.equal (Just "/ldraw/p/1-4cyli.dat")
            , test "subpart prefix routes directly to parts/s/" <|
                \_ ->
                    partCandidateUrls testConfig "s/3001s01.dat"
                        |> Expect.equal
                            [ "/ldraw/parts/s/3001s01.dat"
                            , "https://example.com/ldraw/parts/s/3001s01.dat"
                            ]
            , test "high-res primitive prefix routes directly to p/48/" <|
                \_ ->
                    partCandidateUrls testConfig "48/stud.dat"
                        |> Expect.equal
                            [ "/ldraw/p/48/stud.dat"
                            , "https://example.com/ldraw/p/48/stud.dat"
                            ]
            ]
        ]
