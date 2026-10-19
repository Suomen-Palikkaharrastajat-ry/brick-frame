module LDraw.Format exposing (formatFloat, lineToText, placementToText)

{-| Text formatting shared by `LDraw.Document` and `LDraw.Writer`.

Floats are written in plain decimal notation with at most six decimals, so an
LDraw tool that does not understand exponents can still read the result, and
`parse >> write >> parse` does not drift.

-}

import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)


{-| Plain-decimal float with at most six decimals; `-0` is written as `0`.
-}
formatFloat : Float -> String
formatFloat value =
    let
        rounded =
            toFloat (round (value * 1000000)) / 1000000
    in
    if rounded == 0 then
        "0"

    else
        plainDecimal rounded


plainDecimal : Float -> String
plainDecimal value =
    let
        text =
            String.fromFloat value
    in
    if String.contains "e" text then
        -- Only reachable for |value| >= 1e21, far outside any LDraw model.
        String.fromInt (round value)

    else
        text


vec : Vec3 -> String
vec v =
    String.join " " (List.map formatFloat [ Vec3.getX v, Vec3.getY v, Vec3.getZ v ])


{-| The `x y z a b c d e f g h i` tokens of a type 1 line.
-}
matrixTokens : Mat4 -> List String
matrixTokens matrix =
    let
        m =
            Mat4.toRecord matrix
    in
    List.map formatFloat
        [ m.m14, m.m24, m.m34, m.m11, m.m12, m.m13, m.m21, m.m22, m.m23, m.m31, m.m32, m.m33 ]


{-| One part reference line. Non-empty `extra` makes it a Studio type 11 line.
-}
placementToText : { a | color : Int, matrix : Mat4, fileText : String, extra : List String } -> String
placementToText p =
    let
        ( lineType, extra ) =
            if List.isEmpty p.extra then
                ( "1", [] )

            else
                ( "11", p.extra )
    in
    String.join " " ([ lineType, String.fromInt p.color ] ++ extra ++ matrixTokens p.matrix ++ [ p.fileText ])


{-| Text for a parsed line. Sub-file references are written with the
normalised file name.
-}
lineToText : LDrawLine -> String
lineToText line =
    case line of
        Comment text ->
            if text == "" then
                "0"

            else
                "0 " ++ text

        SubFileRef data ->
            placementToText { color = data.color, matrix = data.transform, fileText = data.file, extra = [] }

        LineSegment d ->
            String.join " " [ "2", String.fromInt d.color, vec d.p1, vec d.p2 ]

        Triangle d ->
            String.join " " [ "3", String.fromInt d.color, vec d.p1, vec d.p2, vec d.p3 ]

        Quad d ->
            String.join " " [ "4", String.fromInt d.color, vec d.p1, vec d.p2, vec d.p3, vec d.p4 ]

        ConditionalLine d ->
            String.join " " [ "5", String.fromInt d.color, vec d.p1, vec d.p2, vec d.c1, vec d.c2 ]
