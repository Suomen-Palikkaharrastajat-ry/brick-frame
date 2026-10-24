module Studio.Layout exposing (Layout, Mode(..), Panel(..), compute, headerHeight, panelWidth, tabBarHeight)

{-| How the studio's panes are laid out for a window, and how big the 3D canvas
inside that layout is.

A header row with the document actions runs across the top in both layouts.
On a wide window the palette sits on the left, the inspector and tree on the
right and the canvas between them. On a narrow one (a phone) the canvas takes the
window above a tab bar and the panels become one at a time, in a sheet over its
lower half.

The canvas size has to be known in Elm — the viewport's camera sensitivity and
picking use it — so the layout is arithmetic rather than something measured from
the DOM.

-}


{-| Wide enough for three panes, or not.
-}
type Mode
    = Wide
    | Narrow


{-| The panels of the narrow layout.
-}
type Panel
    = PartsPanel
    | ColoursPanel
    | InspectorPanel
    | TreePanel


type alias Layout =
    { mode : Mode
    , canvasWidth : Int
    , canvasHeight : Int
    }


{-| Width of each side pane in the wide layout, in pixels.
-}
panelWidth : Int
panelWidth =
    300


{-| Height of the narrow layout's tab bar, taken off the canvas.
-}
tabBarHeight : Int
tabBarHeight =
    46


{-| Height of the header row above the canvas, in pixels.
-}
headerHeight : Mode -> Int
headerHeight mode =
    case mode of
        Wide ->
            56

        Narrow ->
            48


{-| The narrowest window that gets the wide layout.
-}
wideFrom : Int
wideFrom =
    960


{-| The layout for a window of this size.
-}
compute : Int -> Int -> Layout
compute width height =
    if width >= wideFrom then
        { mode = Wide
        , canvasWidth = width - 2 * panelWidth
        , canvasHeight = max 1 (height - headerHeight Wide)
        }

    else
        { mode = Narrow
        , canvasWidth = width
        , canvasHeight = max 1 (height - tabBarHeight - headerHeight Narrow)
        }
