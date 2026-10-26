module UI.SnapshotModal exposing
    ( Actions
    , Model
    , Phase(..)
    , PreviewData
    , Progress
    , init
    , isRendering
    , markSaved
    , setFailed
    , setProgress
    , setReady
    , view
    )

{-| Full-screen modal for capturing, previewing, and saving a snapshot of the
canvas as a 1920 × 1080 PNG.

The modal takes over the whole UI with a blurred backdrop while the offline
path tracer works. It shows the sample count and a progress bar from the
tracer's progress reports (`setProgress`). A failed render shows its error
(`setFailed`). Once the image is ready it appears with a Save button. The user
can close the modal at any time, whether or not the image has been saved.

-}

import Html exposing (Html, button, div, h2, img, p, span, text)
import Html.Attributes as Attr
import Html.Events
import UI.Button as Button
import UI.Icon as Icon
import UI.Theme as Theme


{-| How far the path tracer has got: `fraction` from 0 to 1, and the number of
samples taken so far.
-}
type alias Progress =
    { fraction : Float
    , samples : Int
    }


{-| Data for the preview phase once the captured image is ready.
-}
type alias PreviewData =
    { dataUrl : String
    , filename : String
    , hasSaved : Bool
    }


{-| The phases of snapshot capture: rendering, then either a preview or a failure.
-}
type Phase
    = Rendering
    | Preview PreviewData
    | Failed String


{-| Snapshot modal state.
-}
type alias Model =
    { filename : String
    , phase : Phase
    , progress : Progress
    }


{-| Actions triggered from the modal.
-}
type alias Actions msg =
    { onSave : { dataUrl : String, filename : String } -> msg
    , onClose : msg
    }


{-| Initialize modal in the capturing phase.
-}
init : { filename : String } -> Model
init config =
    { filename = config.filename
    , phase = Rendering
    , progress = { fraction = 0, samples = 0 }
    }


{-| Whether the modal is still waiting for the path tracer. Closing it then
should cancel the render.
-}
isRendering : Model -> Bool
isRendering model =
    case model.phase of
        Rendering ->
            True

        _ ->
            False


{-| Record the path tracer's progress. Ignored once the render has finished or failed.
-}
setProgress : Progress -> Model -> Model
setProgress progress model =
    case model.phase of
        Rendering ->
            { model
                | progress =
                    { fraction = clamp 0 1 progress.fraction
                    , samples = max 0 progress.samples
                    }
            }

        _ ->
            model


{-| Show the error of a failed render. Ignored once the render has finished.
-}
setFailed : String -> Model -> Model
setFailed message model =
    case model.phase of
        Rendering ->
            { model | phase = Failed message }

        _ ->
            model


{-| Transition to the preview phase when JavaScript delivers the captured data URL.
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
                }
    }


{-| Mark the image as saved after download has been initiated.
-}
markSaved : Model -> Model
markSaved model =
    case model.phase of
        Preview data ->
            { model | phase = Preview { data | hasSaved = True } }

        _ ->
            model


{-| Render the modal overlay.
-}
view : Actions msg -> Model -> Html msg
view actions model =
    div
        [ Attr.attribute "role" "dialog"
        , Attr.attribute "aria-modal" "true"
        , Attr.attribute "aria-label" "Snapshot"
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "z-index" "99999"
        , Attr.style "background" scrim
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
            , Attr.style "box-shadow" ("0 25px 50px -12px " ++ shadowColour)
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


{-| The dimmed backdrop behind the dialog, derived from the brand colour.
-}
scrim : String
scrim =
    "color-mix(in srgb, " ++ Theme.brand ++ " 85%, transparent)"


{-| The backdrop behind the captured image, a dark tone from the theme.
-}
previewBackground : String
previewBackground =
    "var(--color-bg-dark)"


{-| The drop shadow colour of the dialog, derived from the brand colour.
-}
shadowColour : String
shadowColour =
    "color-mix(in srgb, " ++ Theme.brand ++ " 75%, transparent)"


viewStyles : Html msg
viewStyles =
    Html.node "style"
        []
        [ text """
@keyframes snapshotSpinAnim {
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
                [ text "Snapshot" ]
            , p
                [ Attr.style "margin" "2px 0 0"
                , Attr.style "font-size" "12px"
                , Attr.style "color" Theme.textMuted
                ]
                [ text "A 1920 × 1080 PNG of the current view" ]
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
            [ text "PNG" ]
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
                [ viewRendering model.progress ]

            Preview data ->
                [ viewPreview data ]

            Failed message ->
                [ viewFailed message ]
        )


viewRendering : Progress -> Html msg
viewRendering progress =
    let
        percent =
            round (progress.fraction * 100)
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "padding" "32px 16px"
        , Attr.style "gap" "16px"
        , Attr.style "text-align" "center"
        ]
        [ div
            [ Attr.style "animation" "snapshotSpinAnim 1.2s linear infinite"
            , Attr.style "display" "flex"
            , Attr.style "color" Theme.brandYellow
            , Attr.attribute "aria-hidden" "true"
            ]
            [ Icon.featherSized 32 "loader" ]
        , div
            [ Attr.style "font-size" "15px"
            , Attr.style "font-weight" "600"
            ]
            [ text ("Path tracing… " ++ String.fromInt progress.samples ++ " samples") ]
        , div
            [ Attr.attribute "role" "progressbar"
            , Attr.attribute "aria-label" "Path tracing progress"
            , Attr.attribute "aria-valuemin" "0"
            , Attr.attribute "aria-valuemax" "100"
            , Attr.attribute "aria-valuenow" (String.fromInt percent)
            , Attr.style "width" "100%"
            , Attr.style "max-width" "420px"
            , Attr.style "height" "8px"
            , Attr.style "background" Theme.panelSubtleBackground
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "9999px"
            , Attr.style "overflow" "hidden"
            ]
            [ div
                [ Attr.style "width" (String.fromInt percent ++ "%")
                , Attr.style "height" "100%"
                , Attr.style "background" Theme.brandYellow
                , Attr.style "transition" "width 0.3s linear"
                ]
                []
            ]
        , p
            [ Attr.style "margin" "0"
            , Attr.style "font-size" "13px"
            , Attr.style "color" Theme.textMuted
            ]
            [ text "Rendering a path-traced image; this takes about 30 seconds." ]
        ]


viewFailed : String -> Html msg
viewFailed message =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "padding" "32px 16px"
        , Attr.style "gap" "12px"
        , Attr.style "text-align" "center"
        ]
        [ div
            [ Attr.style "font-size" "15px"
            , Attr.style "font-weight" "600"
            , Attr.style "color" Theme.brandRed
            ]
            [ text "The snapshot could not be rendered." ]
        , p
            [ Attr.style "margin" "0"
            , Attr.style "font-size" "13px"
            , Attr.style "color" Theme.textMuted
            ]
            [ text message ]
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
            , Attr.style "background" previewBackground
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "8px"
            , Attr.style "overflow" "hidden"
            ]
            [ img
                [ Attr.src data.dataUrl
                , Attr.alt "Snapshot preview"
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
                [ text (data.filename ++ " • PNG") ]
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
                    [ text "The image will appear here when it is ready." ]
                , Button.secondary actions.onClose "Close"
                ]

            Preview data ->
                [ div
                    [ Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "10px"
                    ]
                    [ saveButton actions data ]
                , Button.secondary actions.onClose "Close"
                ]

            Failed _ ->
                [ div
                    [ Attr.style "font-size" "13px"
                    , Attr.style "color" Theme.textMuted
                    ]
                    [ text "Nothing was saved." ]
                , Button.secondary actions.onClose "Close"
                ]
        )


saveButton : Actions msg -> PreviewData -> Html msg
saveButton actions data =
    button
        [ Attr.type_ "button"
        , Html.Events.onClick (actions.onSave { dataUrl = data.dataUrl, filename = data.filename })
        , Attr.class "ui-button touch-target"
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "padding" "8px 16px"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "6px"
        , Attr.style "font-weight" "600"
        , Attr.style "font-size" "13px"
        , Attr.style "cursor" "pointer"
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
