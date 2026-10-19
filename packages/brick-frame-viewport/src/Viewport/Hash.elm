module Viewport.Hash exposing
    ( HashState
    , decode
    , encodeCamera
    , hasExplicitCamera
    , navigationFromString
    , navigationToString
    )

{-| The camera as it appears in a URL hash (`#az=..&el=..&d=..`).

Pure string handling, so hosts can read the initial hash before creating a
`Viewport` and write the camera back with whatever port they own.

-}

import Dict
import Math.Vector3 as Vec3
import Render.Camera as Camera exposing (Camera)


type alias HashState =
    { azimuth : Maybe Float
    , elevation : Maybe Float
    , distance : Maybe Float
    , targetX : Maybe Float
    , targetY : Maybe Float
    , targetZ : Maybe Float
    , navigation : Maybe Camera.Navigation
    }


decode : String -> HashState
decode rawHash =
    let
        withoutPrefix =
            if String.startsWith "#" rawHash then
                String.dropLeft 1 rawHash

            else
                rawHash

        entries =
            withoutPrefix
                |> String.split "&"
                |> List.filter (\chunk -> String.trim chunk /= "")
                |> List.filterMap
                    (\chunk ->
                        case String.split "=" chunk of
                            key :: rest ->
                                Just ( key, String.join "=" rest )

                            [] ->
                                Nothing
                    )
                |> Dict.fromList

        getFloat key =
            Dict.get key entries |> Maybe.andThen String.toFloat
    in
    { azimuth = getFloat "az"
    , elevation = getFloat "el"
    , distance = getFloat "d"
    , targetX = getFloat "tx"
    , targetY = getFloat "ty"
    , targetZ = getFloat "tz"
    , navigation = Dict.get "nav" entries |> Maybe.andThen navigationFromString
    }


hasExplicitCamera : HashState -> Bool
hasExplicitCamera state =
    state.azimuth
        /= Nothing
        || state.elevation
        /= Nothing
        || state.distance
        /= Nothing
        || state.targetX
        /= Nothing
        || state.targetY
        /= Nothing
        || state.targetZ
        /= Nothing
        || state.navigation
        /= Nothing


encodeCamera : Camera -> String
encodeCamera camera =
    "az="
        ++ String.fromFloat camera.azimuth
        ++ "&el="
        ++ String.fromFloat camera.elevation
        ++ "&d="
        ++ String.fromFloat camera.distance
        ++ "&tx="
        ++ String.fromFloat (Vec3.getX camera.target)
        ++ "&ty="
        ++ String.fromFloat (Vec3.getY camera.target)
        ++ "&tz="
        ++ String.fromFloat (Vec3.getZ camera.target)
        -- Orbit is the default, so only walk needs saying. Keeps the common
        -- case's URLs as short as they were.
        ++ (case camera.navigation of
                Camera.Orbit ->
                    ""

                Camera.Walk ->
                    "&nav=walk"
           )


{-| Navigation mode name used in the URL hash, the `camera-changed` payload and
the `camera-navigation` attribute.
-}
navigationToString : Camera.Navigation -> String
navigationToString navigation =
    case navigation of
        Camera.Orbit ->
            "orbit"

        Camera.Walk ->
            "walk"


{-| Parse a navigation mode name, ignoring case. Unknown values are `Nothing`, so
a typo falls back to auto-fit orbit rather than silently walking.
-}
navigationFromString : String -> Maybe Camera.Navigation
navigationFromString raw =
    case String.toLower (String.trim raw) of
        "walk" ->
            Just Camera.Walk

        "orbit" ->
            Just Camera.Orbit

        _ ->
            Nothing
