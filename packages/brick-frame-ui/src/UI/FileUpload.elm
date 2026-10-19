module UI.FileUpload exposing (Config, view)

{-| Upload control for loading local LDraw and Studio files.
-}

import Html exposing (Html)
import UI.Button as Button


type alias Config msg =
    { onRequestFileUpload : msg }


view : Config msg -> List (Html msg)
view config =
    [ Button.secondary config.onRequestFileUpload "Upload File" ]
