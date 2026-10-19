module UI.Status exposing (Config, Phase(..), view)

{-| The centred message box over the canvas: a loading bar while a model is being
prepared, or an error with a Dismiss button.

Takes plain values rather than a `Viewport.Model` so this package stays free of
the viewport; each application maps its own load state onto `Phase`.

-}

import Html exposing (Html, button, div, text)
import Html.Attributes as Attr
import Html.Events
import UI.Theme as Theme


{-| Where loading is. Percentages are 0–100.
-}
type Phase
    = Done
    | Fetching
    | ResolvingParts { loaded : Int, total : Int, percent : Float }
    | Finalizing Float


{-| An error replaces the loading display; `onDismiss` clears it.
-}
type alias Config msg =
    { error : Maybe String
    , phase : Phase
    , onDismiss : msg
    }


view : Config msg -> Html msg
view config =
    case config.error of
        Just err ->
            div
                [ Attr.style "position" "absolute"
                , Attr.style "top" "50%"
                , Attr.style "left" "50%"
                , Attr.style "transform" "translate(-50%, -50%)"
                , Attr.style "color" Theme.brandRed
                , Attr.style "font-family" "monospace"
                , Attr.style "font-size" "14px"
                , Attr.style "text-align" "center"
                , Attr.style "background" Theme.panelSurface
                , Attr.style "padding" "16px 24px"
                , Attr.style "border-radius" "8px"
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "box-shadow" "0 16px 48px color-mix(in srgb, var(--color-brand) 12%, transparent)"
                , Attr.style "pointer-events" "auto"
                ]
                [ div [] [ text err ]
                , button
                    [ Html.Events.onClick config.onDismiss
                    , Attr.style "margin-top" "10px"
                    , Attr.style "padding" "4px 12px"
                    , Attr.style "background" Theme.panelSubtleBackground
                    , Attr.style "color" Theme.textPrimary
                    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                    , Attr.style "border-radius" "4px"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "font-family" "monospace"
                    , Attr.style "font-size" "12px"
                    ]
                    [ text "Dismiss" ]
                ]

        Nothing ->
            case config.phase of
                Done ->
                    text ""

                Fetching ->
                    viewLoadingBox "Fetching model…" Nothing

                ResolvingParts { loaded, total, percent } ->
                    viewLoadingBox
                        ("Preparing model… "
                            ++ String.fromInt loaded
                            ++ " / "
                            ++ String.fromInt total
                        )
                        (Just (round percent))

                Finalizing percent ->
                    viewLoadingBox
                        "Preparing model… finalizing geometry"
                        (Just (round percent))


viewLoadingBox : String -> Maybe Int -> Html msg
viewLoadingBox label maybePct =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "50%"
        , Attr.style "left" "50%"
        , Attr.style "transform" "translate(-50%, -50%)"
        , Attr.style "color" Theme.textPrimary
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "13px"
        , Attr.style "text-align" "center"
        , Attr.style "background" Theme.panelSurface
        , Attr.style "padding" "20px 28px"
        , Attr.style "border-radius" "8px"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "box-shadow" "0 16px 48px color-mix(in srgb, var(--color-brand) 10%, transparent)"
        , Attr.style "min-width" "260px"
        ]
        (div [] [ text label ]
            :: (case maybePct of
                    Just pct ->
                        [ div
                            [ Attr.style "margin-top" "10px"
                            , Attr.style "height" "4px"
                            , Attr.style "background" Theme.panelSubtleBackground
                            , Attr.style "border-radius" "2px"
                            , Attr.style "overflow" "hidden"
                            ]
                            [ div
                                [ Attr.style "height" "100%"
                                , Attr.style "width" (String.fromInt pct ++ "%")
                                , Attr.style "background" Theme.brandYellow
                                , Attr.style "border-radius" "2px"
                                , Attr.style "transition" "width 0.2s"
                                ]
                                []
                            ]
                        ]

                    Nothing ->
                        []
               )
        )
