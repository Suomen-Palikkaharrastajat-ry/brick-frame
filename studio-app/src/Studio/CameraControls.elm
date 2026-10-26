module Studio.CameraControls exposing (Actions, floating, view)

{-| The camera controls every studio page shows: the shared camera globe
(`UI.CameraGlobe`) with the street-level walk and keyboard help buttons beside
its zoom slider. The pages keep their own `Msg` and their own pointer state; this
module only arranges the controls and tags each one with the page's messages.
-}

import Html exposing (Html, div)
import Html.Attributes as Attr
import Render.Camera as Camera exposing (Camera)
import UI.Button as Button
import UI.CameraGlobe as CameraGlobe
import UI.Focus as Focus
import UI.Icon as Icon


{-| The messages a page hands over for each control.
`noOp` is sent by nothing; it only keeps focus off the buttons after a mouse
press, so Space keeps meaning play.
-}
type alias Actions msg =
    { onPress : ( Float, Float ) -> msg
    , onMove : ( Float, Float ) -> msg
    , onRelease : msg
    , onZoom : Float -> msg
    , onZoomDone : msg
    , onReset : msg
    , onWalk : msg
    , onHelp : msg
    , noOp : msg
    }


{-| The globe and its buttons, to sit where the page puts them.
-}
view : Actions msg -> { camera : Camera, helpOpen : Bool } -> Html msg
view actions state =
    CameraGlobe.view
        { camera = state.camera
        , onPress = actions.onPress
        , onMove = actions.onMove
        , onRelease = actions.onRelease
        , onZoom = actions.onZoom
        , onZoomDone = actions.onZoomDone
        , onReset = actions.onReset
        , extras = [ extraButtons actions state ]
        }


{-| `view` pinned to the bottom-right corner of the canvas, as the pads were.
-}
floating : Actions msg -> { camera : Camera, helpOpen : Bool } -> Html msg
floating actions state =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "right" "12px"
        , Attr.style "bottom" "12px"
        ]
        [ view actions state ]


extraButtons : Actions msg -> { camera : Camera, helpOpen : Bool } -> Html msg
extraButtons actions state =
    div
        [ Focus.keepFocusOnCanvas actions.noOp
        , Attr.style "display" "contents"
        ]
        [ Button.icon
            { icon = Icon.feather "user"
            , label = "Street level view"
            , shortcut = Just "G"
            , pressed = Just (state.camera.navigation == Camera.Walk)
            , enabled = True
            , showLabel = False
            }
            actions.onWalk
        , Button.icon
            { icon = Icon.feather "help-circle"
            , label = "Keyboard shortcuts"
            , shortcut = Just "?"
            , pressed = Just state.helpOpen
            , enabled = True
            , showLabel = False
            }
            actions.onHelp
        ]
