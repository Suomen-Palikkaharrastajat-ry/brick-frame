module UI.FileUpload exposing (Config, view)

{-| URL input and upload controls for loading LDraw models.
-}

import Html exposing (Html, button, input, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode


type alias Config msg =
    { urlInput : String
    , onUrlInput : String -> msg
    , onLoadUrl : msg
    , onRequestFileUpload : msg
    }


view : Config msg -> List (Html msg)
view config =
    [ input
        [ Attr.type_ "text"
        , Attr.value config.urlInput
        , Attr.placeholder "LDraw / Studio URL (.ldr / .mpd / .io)"
        , Html.Events.onInput config.onUrlInput
        , Html.Events.on "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed config.onLoadUrl

                        else
                            Decode.fail "not enter"
                    )
            )
        , Attr.style "flex" "1"
        , Attr.style "padding" "6px 10px"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "background" "rgba(255,255,255,0.1)"
        , Attr.style "border" "1px solid rgba(255,255,255,0.25)"
        , Attr.style "border-radius" "4px"
        , Attr.style "color" "#fff"
        , Attr.style "outline" "none"
        ]
        []
    , button
        [ Html.Events.onClick config.onLoadUrl
        , Attr.style "padding" "6px 14px"
        , Attr.style "background" "#f5c518"
        , Attr.style "color" "#1a1a2e"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "4px"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "cursor" "pointer"
        ]
        [ text "Load URL" ]
    , button
        [ Html.Events.onClick config.onRequestFileUpload
        , Attr.style "padding" "6px 14px"
        , Attr.style "background" "rgba(255,255,255,0.15)"
        , Attr.style "color" "#fff"
        , Attr.style "border" "1px solid rgba(255,255,255,0.3)"
        , Attr.style "border-radius" "4px"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "cursor" "pointer"
        ]
        [ text "Upload File" ]
    ]
