module UI.Icon exposing (feather)

{-| The handful of Feather icons the controls use, looked up by name.
-}

import FeatherIcons
import Html exposing (Html)
import Html.Attributes as Attr


feather : String -> Html msg
feather name =
    let
        icon =
            case name of
                "chevron-up" ->
                    FeatherIcons.chevronUp

                "chevron-down" ->
                    FeatherIcons.chevronDown

                "chevron-left" ->
                    FeatherIcons.chevronLeft

                "chevron-right" ->
                    FeatherIcons.chevronRight

                "pause" ->
                    FeatherIcons.pause

                "maximize" ->
                    FeatherIcons.maximize

                "user" ->
                    FeatherIcons.user

                "help-circle" ->
                    FeatherIcons.helpCircle

                "x" ->
                    FeatherIcons.x

                _ ->
                    FeatherIcons.play
    in
    icon
        |> FeatherIcons.withSize 14
        |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
