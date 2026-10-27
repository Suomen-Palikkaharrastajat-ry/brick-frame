module UI.Icon exposing (feather, featherSized)

{-| The handful of Feather icons the controls use, looked up by name.
`feather` draws them at 14 px; `featherSized` at a given size, for larger
touch buttons.
-}

import FeatherIcons
import Html exposing (Html)
import Html.Attributes as Attr


feather : String -> Html msg
feather =
    featherSized 14


featherSized : Int -> String -> Html msg
featherSized size name =
    let
        icon =
            case name of
                "chevron-up" ->
                    FeatherIcons.chevronUp

                "chevron-down" ->
                    FeatherIcons.chevronDown

                "chevrons-up" ->
                    FeatherIcons.chevronsUp

                "chevrons-down" ->
                    FeatherIcons.chevronsDown

                "chevron-left" ->
                    FeatherIcons.chevronLeft

                "chevron-right" ->
                    FeatherIcons.chevronRight

                "pause" ->
                    FeatherIcons.pause

                "crosshair" ->
                    FeatherIcons.crosshair

                "maximize" ->
                    FeatherIcons.maximize

                "user" ->
                    FeatherIcons.user

                "help-circle" ->
                    FeatherIcons.helpCircle

                "x" ->
                    FeatherIcons.x

                "rotate-ccw" ->
                    FeatherIcons.rotateCcw

                "rotate-cw" ->
                    FeatherIcons.rotateCw

                "corner-right-down" ->
                    FeatherIcons.cornerRightDown

                "arrow-up" ->
                    FeatherIcons.arrowUp

                "arrow-down" ->
                    FeatherIcons.arrowDown

                "trash-2" ->
                    FeatherIcons.trash2

                "menu" ->
                    FeatherIcons.menu

                "sun" ->
                    FeatherIcons.sun

                "camera" ->
                    FeatherIcons.camera

                "download" ->
                    FeatherIcons.download

                "check" ->
                    FeatherIcons.check

                "loader" ->
                    FeatherIcons.loader

                "aperture" ->
                    FeatherIcons.aperture

                "lock" ->
                    FeatherIcons.lock

                "unlock" ->
                    FeatherIcons.unlock

                "grid" ->
                    FeatherIcons.grid

                "columns" ->
                    FeatherIcons.columns

                _ ->
                    FeatherIcons.play
    in
    icon
        |> FeatherIcons.withSize (toFloat size)
        |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
