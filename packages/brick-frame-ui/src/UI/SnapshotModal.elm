module UI.SnapshotModal exposing
    ( Actions
    , Model
    , Phase(..)
    , PreviewData
    , canContinue
    , init
    , markSaved
    , setAgreed
    , setReady
    , view
    )

{-| Full-screen modal for taking, previewing, and saving high-quality raytraced screenshots.

Takes over the whole UI with a blurred backdrop while rendering completes (~1.5s–2s)
and allows continuing only after saving the render or agreeing/confirming review.

-}

import Html exposing (Html, button, div, h2, img, input, label, p, span, text)
import Html.Attributes as Attr
import Html.Events
import UI.Button as Button
import UI.Icon as Icon
import UI.Theme as Theme


{-| Data for the preview phase once the high-resolution render is ready.
-}
type alias PreviewData =
    { dataUrl : String
    , filename : String
    , hasSaved : Bool
    , hasAgreed : Bool
    }


{-| The two phases of screenshot capture.
-}
type Phase
    = Rendering
    | Preview PreviewData


{-| Snapshot modal state.
-}
type alias Model =
    { filename : String
    , phase : Phase
    }


{-| Actions triggered from the modal.
-}
type alias Actions msg =
    { onSave : { dataUrl : String, filename : String } -> msg
    , onToggleAgreed : Bool -> msg
    , onContinue : msg
    }


{-| Initialize modal in the rendering phase.
-}
init : { filename : String } -> Model
init config =
    { filename = config.filename
    , phase = Rendering
    }


{-| Transition to the preview phase when JavaScript delivers the rendered data URL.
-}
setReady : { dataUrl : String, filename : String } -> Model -> Model
setReady { dataUrl, filename } model =
    { model
        | filename = filename
        , phase =
            Preview
                { dataUrl = dataUrl
                , filename = filename
                , hasSaved = False
                , hasAgreed = False
                }
    }


{-| Mark the image as saved after download has been initiated.
-}
markSaved : Model -> Model
markSaved model =
    case model.phase of
        Preview data ->
            { model | phase = Preview { data | hasSaved = True } }

        Rendering ->
            model


{-| Update user agreement checkbox.
-}
setAgreed : Bool -> Model -> Model
setAgreed agreed model =
    case model.phase of
        Preview data ->
            { model | phase = Preview { data | hasAgreed = agreed } }

        Rendering ->
            model


{-| Whether the user is permitted to dismiss the modal and continue.
-}
canContinue : Model -> Bool
canContinue model =
    case model.phase of
        Preview data ->
            data.hasSaved || data.hasAgreed

        Rendering ->
            False


{-| Render the modal overlay.
-}
view : Actions msg -> Model -> Html msg
view actions model =
    div
        [ Attr.attribute "role" "dialog"
        , Attr.attribute "aria-modal" "true"
        , Attr.attribute "aria-label" "Raytraced snapshot"
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "z-index" "99999"
        , Attr.style "background" "rgba(10, 15, 29, 0.85)"
        , Attr.style "backdrop-filter" "blur(12px)"
        , Attr.style "-webkit-backdrop-filter" "blur(12px)"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "padding" "20px"
        , Attr.style "box-sizing" "border-box"
        ]
        [ viewStyles
        , div
            [ Attr.style "background" Theme.panelSurface
            , Attr.style "color" Theme.textPrimary
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "12px"
            , Attr.style "box-shadow" "0 25px 50px -12px rgba(0, 0, 0, 0.75)"
            , Attr.style "max-width" "840px"
            , Attr.style "width" "100%"
            , Attr.style "max-height" "calc(100vh - 40px)"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "overflow" "hidden"
            ]
            [ viewHeader
            , viewBody model
            , viewFooter actions model
            ]
        ]


viewStyles : Html msg
viewStyles =
    Html.node "style"
        []
        [ text """
@keyframes raytraceProgressAnim {
    0% { width: 5%; }
    20% { width: 35%; }
    55% { width: 70%; }
    80% { width: 90%; }
    100% { width: 98%; }
}
@keyframes raytraceSpinAnim {
    from { transform: rotate(0deg); }
    to { transform: rotate(360deg); }
}
"""
        ]


viewHeader : Html msg
viewHeader =
    div
        [ Attr.style "padding" "16px 20px"
        , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        ]
        [ div []
            [ h2
                [ Attr.style "margin" "0"
                , Attr.style "font-size" "15px"
                , Attr.style "font-weight" "600"
                , Attr.style "letter-spacing" "-0.01em"
                ]
                [ text "High-Quality Raytraced Snapshot" ]
            , p
                [ Attr.style "margin" "2px 0 0"
                , Attr.style "font-size" "12px"
                , Attr.style "color" Theme.textMuted
                ]
                [ text "Realistic rendering with global illumination and ambient occlusion" ]
            ]
        , span
            [ Attr.style "font-size" "11px"
            , Attr.style "font-weight" "600"
            , Attr.style "padding" "3px 8px"
            , Attr.style "border-radius" "9999px"
            , Attr.style "background" Theme.panelSubtleBackground
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "color" Theme.textMuted
            ]
            [ text "1080p (1920 × 1080)" ]
        ]


viewBody : Model -> Html msg
viewBody model =
    div
        [ Attr.style "padding" "24px 20px"
        , Attr.style "overflow-y" "auto"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "16px"
        ]
        (case model.phase of
            Rendering ->
                [ viewRendering ]

            Preview data ->
                [ viewPreview data ]
        )


viewRendering : Html msg
viewRendering =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "padding" "48px 16px"
        , Attr.style "gap" "16px"
        , Attr.style "text-align" "center"
        ]
        [ div
            [ Attr.style "animation" "raytraceSpinAnim 1.2s linear infinite"
            , Attr.style "display" "flex"
            , Attr.style "color" Theme.brandYellow
            ]
            [ Icon.featherSized 32 "loader" ]
        , div []
            [ div
                [ Attr.style "font-size" "15px"
                , Attr.style "font-weight" "600"
                , Attr.style "margin-bottom" "4px"
                ]
                [ text "Raytracing realistic rendering..." ]
            , div
                [ Attr.style "font-size" "13px"
                , Attr.style "color" Theme.textMuted
                ]
                [ text "Accumulating samples, raytracing global illumination & ambient occlusion" ]
            ]
        , div
            [ Attr.style "width" "100%"
            , Attr.style "max-width" "420px"
            , Attr.style "height" "8px"
            , Attr.style "background" "rgba(255, 255, 255, 0.1)"
            , Attr.style "border-radius" "4px"
            , Attr.style "overflow" "hidden"
            , Attr.style "margin-top" "8px"
            ]
            [ div
                [ Attr.style "height" "100%"
                , Attr.style "background" Theme.brandYellow
                , Attr.style "border-radius" "4px"
                , Attr.style "animation" "raytraceProgressAnim 1.8s ease-out forwards"
                ]
                []
            ]
        ]


viewPreview : PreviewData -> Html msg
viewPreview data =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "12px"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "background" "#0b0f19"
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "8px"
            , Attr.style "overflow" "hidden"
            ]
            [ img
                [ Attr.src data.dataUrl
                , Attr.alt "High-quality raytraced render preview"
                , Attr.style "max-height" "460px"
                , Attr.style "width" "100%"
                , Attr.style "object-fit" "contain"
                , Attr.style "display" "block"
                ]
                []
            ]
        , div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "space-between"
            , Attr.style "font-size" "12px"
            , Attr.style "color" Theme.textMuted
            ]
            [ div []
                [ text (data.filename ++ " • 1080p high quality PNG") ]
            , if data.hasSaved then
                div
                    [ Attr.style "display" "inline-flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "6px"
                    , Attr.style "color" Theme.brandYellow
                    , Attr.style "font-weight" "500"
                    ]
                    [ Icon.featherSized 14 "check"
                    , text "Saved to downloads"
                    ]

              else
                div [] [ text "Ready to save" ]
            ]
        ]


viewFooter : Actions msg -> Model -> Html msg
viewFooter actions model =
    div
        [ Attr.style "padding" "14px 20px"
        , Attr.style "border-top" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "gap" "12px"
        , Attr.style "flex-wrap" "wrap"
        ]
        (case model.phase of
            Rendering ->
                [ div
                    [ Attr.style "font-size" "13px"
                    , Attr.style "color" Theme.textMuted
                    ]
                    [ text "Please wait while the snapshot renders..." ]
                , Button.action { enabled = False } actions.onContinue "Continue"
                ]

            Preview data ->
                let
                    allowed =
                        canContinue model
                in
                [ label
                    [ Attr.style "display" "inline-flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "8px"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "font-size" "13px"
                    , Attr.style "user-select" "none"
                    , Attr.style "color" Theme.textPrimary
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.checked data.hasAgreed
                        , Html.Events.onCheck actions.onToggleAgreed
                        , Attr.style "cursor" "pointer"
                        , Attr.style "width" "16px"
                        , Attr.style "height" "16px"
                        ]
                        []
                    , text "I have saved / reviewed the render"
                    ]
                , div
                    [ Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "10px"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Html.Events.onClick (actions.onSave { dataUrl = data.dataUrl, filename = data.filename })
                        , Attr.class "ui-button touch-target"
                        , Attr.style "display" "inline-flex"
                        , Attr.style "align-items" "center"
                        , Attr.style "gap" "8px"
                        , Attr.style "padding" "8px 16px"
                        , Attr.style "background"
                            (if data.hasSaved then
                                Theme.panelSubtleBackground

                             else
                                Theme.brandYellow
                            )
                        , Attr.style "color"
                            (if data.hasSaved then
                                Theme.textPrimary

                             else
                                Theme.brand
                            )
                        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                        , Attr.style "border-radius" "6px"
                        , Attr.style "font-weight" "600"
                        , Attr.style "font-size" "13px"
                        , Attr.style "cursor" "pointer"
                        ]
                        [ Icon.feather
                            (if data.hasSaved then
                                "check"

                             else
                                "download"
                            )
                        , text
                            (if data.hasSaved then
                                "Saved Image"

                             else
                                "Save Image"
                            )
                        ]
                    , if allowed then
                        Button.primary actions.onContinue "Continue"

                      else
                        Button.action { enabled = False } actions.onContinue "Continue"
                    ]
                ]
        )
