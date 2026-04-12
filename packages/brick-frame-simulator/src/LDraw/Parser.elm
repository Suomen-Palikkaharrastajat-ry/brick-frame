module LDraw.Parser exposing (firstMpdFileName, parseFile, parseLine, splitMpd)

{-| LDraw file format parser.

Converts raw LDraw text (from a `.ldr`, `.mpd`, or `.dat` file) into
typed `LDrawLine` values. All parsing is pure — no IO involved.

Reference: <https://www.ldraw.org/article/218.html>

-}

import Dict exposing (Dict)
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)


-- ── Public API ────────────────────────────────────────────────────────────────


{-| Parse a complete LDraw file text into a list of typed lines.
Lines that are empty, whitespace-only, or malformed are silently dropped.
-}
parseFile : String -> List LDrawLine
parseFile text =
    text
        |> String.lines
        |> List.filterMap parseLine


{-| Parse a single LDraw text line. Returns `Nothing` for empty, whitespace,
or malformed lines.
-}
parseLine : String -> Maybe LDrawLine
parseLine raw =
    case String.words raw of
        [] ->
            Nothing

        typeStr :: rest ->
            case typeStr of
                "0" ->
                    Just (Comment (String.join " " rest))

                "1" ->
                    parseSubFileRef rest

                "11" ->
                    parseSubFileRefV2 rest

                "2" ->
                    parseLineSegment rest

                "3" ->
                    parseTriangle rest

                "4" ->
                    parseQuad rest

                "5" ->
                    parseConditional rest

                _ ->
                    Nothing


{-| Split an MPD file into its embedded sub-files.

An MPD file contains multiple LDraw files separated by `0 FILE <name>`
meta-commands. Returns a `Dict` mapping each embedded filename to its
content (the lines between the FILE markers, not including the marker itself).

Filenames in the returned Dict are normalised (lowercase, forward slashes).

-}
splitMpd : String -> Dict String String
splitMpd text =
    let
        lines =
            String.lines text

        step line ( currentName, currentLines, acc ) =
            case parseFileName line of
                Just name ->
                    -- Flush current section, start new one
                    let
                        flushed =
                            case currentName of
                                Just n ->
                                    Dict.insert n (String.join "\n" (List.reverse currentLines)) acc

                                Nothing ->
                                    acc
                    in
                    ( Just name, [], flushed )

                Nothing ->
                    ( currentName, line :: currentLines, acc )

        ( lastName, lastLines, result ) =
            List.foldl step ( Nothing, [], Dict.empty ) lines
    in
    case lastName of
        Just n ->
            Dict.insert n (String.join "\n" (List.reverse lastLines)) result

        Nothing ->
            result


{-| Return the first embedded file name in an MPD (`0 FILE <name>`), normalised
the same way as `splitMpd` keys.
-}
firstMpdFileName : String -> Maybe String
firstMpdFileName text =
    text
        |> String.lines
        |> List.filterMap parseFileName
        |> List.head


-- ── Internal parsers ──────────────────────────────────────────────────────────


parseFileName : String -> Maybe String
parseFileName line =
    let
        ws =
            String.words line
    in
    case ws of
        "0" :: "FILE" :: rest ->
            Just (normaliseName (String.join " " rest))

        _ ->
            Nothing


parseSubFileRef : List String -> Maybe LDrawLine
parseSubFileRef tokens =
    -- color x y z a b c d e f g h i file
    case tokens of
        [ c, x, y, z, a, b, d, e, f, g, h, i, j, file ] ->
            Maybe.map5
                (\color tx ty tz rot ->
                    SubFileRef
                        { color = color
                        , transform =
                            Mat4.fromRecord
                                { m11 = getF 0 rot
                                , m21 = getF 3 rot
                                , m31 = getF 6 rot
                                , m41 = 0
                                , m12 = getF 1 rot
                                , m22 = getF 4 rot
                                , m32 = getF 7 rot
                                , m42 = 0
                                , m13 = getF 2 rot
                                , m23 = getF 5 rot
                                , m33 = getF 8 rot
                                , m43 = 0
                                , m14 = tx
                                , m24 = ty
                                , m34 = tz
                                , m44 = 1
                                }
                        , file = normaliseName file
                        }
                )
                (String.toInt c)
                (String.toFloat x)
                (String.toFloat y)
                (String.toFloat z)
                (floatList [ a, b, d, e, f, g, h, i, j ])

        _ ->
            Nothing


parseSubFileRefV2 : List String -> Maybe LDrawLine
parseSubFileRefV2 tokens =
    -- Studio v2 type 11: color uid hiddenFlag step x y z a..i file
    case tokens of
        color :: _ :: _ :: _ :: rest ->
            parseSubFileRef (color :: rest)

        _ ->
            Nothing


parseLineSegment : List String -> Maybe LDrawLine
parseLineSegment tokens =
    case tokens of
        [ c, x1, y1, z1, x2, y2, z2 ] ->
            Maybe.map2
                (\color pts ->
                    LineSegment
                        { color = color
                        , p1 = vec3 (getF 0 pts) (getF 1 pts) (getF 2 pts)
                        , p2 = vec3 (getF 3 pts) (getF 4 pts) (getF 5 pts)
                        }
                )
                (String.toInt c)
                (floatList [ x1, y1, z1, x2, y2, z2 ])

        _ ->
            Nothing


parseTriangle : List String -> Maybe LDrawLine
parseTriangle tokens =
    case tokens of
        [ c, x1, y1, z1, x2, y2, z2, x3, y3, z3 ] ->
            Maybe.map2
                (\color pts ->
                    Triangle
                        { color = color
                        , p1 = vec3 (getF 0 pts) (getF 1 pts) (getF 2 pts)
                        , p2 = vec3 (getF 3 pts) (getF 4 pts) (getF 5 pts)
                        , p3 = vec3 (getF 6 pts) (getF 7 pts) (getF 8 pts)
                        }
                )
                (String.toInt c)
                (floatList [ x1, y1, z1, x2, y2, z2, x3, y3, z3 ])

        _ ->
            Nothing


parseQuad : List String -> Maybe LDrawLine
parseQuad tokens =
    case tokens of
        [ c, x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4 ] ->
            Maybe.map2
                (\color pts ->
                    Quad
                        { color = color
                        , p1 = vec3 (getF 0 pts) (getF 1 pts) (getF 2 pts)
                        , p2 = vec3 (getF 3 pts) (getF 4 pts) (getF 5 pts)
                        , p3 = vec3 (getF 6 pts) (getF 7 pts) (getF 8 pts)
                        , p4 = vec3 (getF 9 pts) (getF 10 pts) (getF 11 pts)
                        }
                )
                (String.toInt c)
                (floatList [ x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4 ])

        _ ->
            Nothing


parseConditional : List String -> Maybe LDrawLine
parseConditional tokens =
    case tokens of
        [ c, x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4 ] ->
            Maybe.map2
                (\color pts ->
                    ConditionalLine
                        { color = color
                        , p1 = vec3 (getF 0 pts) (getF 1 pts) (getF 2 pts)
                        , p2 = vec3 (getF 3 pts) (getF 4 pts) (getF 5 pts)
                        , c1 = vec3 (getF 6 pts) (getF 7 pts) (getF 8 pts)
                        , c2 = vec3 (getF 9 pts) (getF 10 pts) (getF 11 pts)
                        }
                )
                (String.toInt c)
                (floatList [ x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4 ])

        _ ->
            Nothing


-- ── Helpers ───────────────────────────────────────────────────────────────────


{-| Parse a list of strings to floats. Returns Nothing if any fail.
-}
floatList : List String -> Maybe (List Float)
floatList strs =
    let
        parsed =
            List.filterMap String.toFloat strs
    in
    if List.length parsed == List.length strs then
        Just parsed

    else
        Nothing


{-| Safe index into a list of floats. Returns 0.0 if out of bounds.
-}
getF : Int -> List Float -> Float
getF idx lst =
    lst
        |> List.drop idx
        |> List.head
        |> Maybe.withDefault 0.0


{-| Normalise an LDraw filename: lowercase, backslash → forward slash,
strip any leading path component (LDraw files often embed paths like
`parts\stud.dat` or `s\stud4.dat`).
-}
normaliseName : String -> String
normaliseName raw =
    raw
        |> String.toLower
        |> String.replace "\\" "/"
        |> String.trim
        |> stripLeadingSegments


stripLeadingSegments : String -> String
stripLeadingSegments value =
    if String.startsWith "./" value then
        stripLeadingSegments (String.dropLeft 2 value)

    else if String.startsWith "/" value then
        stripLeadingSegments (String.dropLeft 1 value)

    else if String.startsWith "ldraw/" value then
        stripLeadingSegments (String.dropLeft 6 value)

    else if String.startsWith "parts/s/" value then
        "s/" ++ String.dropLeft 8 value

    else if String.startsWith "parts/" value then
        String.dropLeft 6 value

    else if String.startsWith "p/48/" value then
        "48/" ++ String.dropLeft 5 value

    else if String.startsWith "p/" value then
        String.dropLeft 2 value

    else if String.startsWith "models/" value then
        String.dropLeft 7 value

    else
        value
