module UI.PartCard exposing (Badge, CompactConfig, Config, compactPanelHeight, compactView, thumbnailKey, view)

{-| A visual part card for the dark floating docks of Brick Play and Brick
Contraptions: a rendered image of the part with its short name below and a
line of small secondary text (a part number, or how many are left).

The images come from the shared thumbnail pipeline
(`packages/brick-frame-viewport/runtime/thumbnail-pipeline.js`). A host

1.  declares the two ports in its `Ports.elm`:

        port requestThumbnails : List { file : String, color : Int, rgb : List Float } -> Cmd msg

        port thumbnailLoaded : ({ file : String, color : Int, url : String } -> msg) -> Sub msg

2.  calls `initThumbnails(app.ports, ldrawBase)` in its `main.js`;

3.  asks for the parts it lists, with `rgb` from
    `LDraw.Colors.resolveColor color 16` (the same colour table the 3D view
    uses), and keeps each answer in a `Dict` under `thumbnailKey file color`;

4.  renders each part with `view`, passing `Dict.get (thumbnailKey file color)`
    as `imageUrl`.

The image sits on a soft light tile (`backdrop`), so black and dark parts
(axles, friction pins) stand out from the dark dock while white and light grey
parts still read against its mid-light grey.

Until an image arrives, or when a part cannot be rendered, the card shows a
neutral placeholder of the same size, so the dock never jumps.

The whole card is one button. `onClick = Nothing` disables it (for example when
none of the part is left): it stays solid, with a greyed image and the secondary
text in the warning colour.

`compactView` is the picture-only card for the minimal docks: a smaller image,
no caption (its name and part number go in `tooltip` and `ariaLabel`), and an
optional count `Badge` on the image corner. The dock that holds compact cards is
`compactPanelHeight` tall.

-}

import Html exposing (Html, button, div, img, span, text)
import Html.Attributes as Attr
import Html.Events


{-| What a card shows and does.

  - `imageUrl`: the rendered thumbnail, `Nothing` while loading or on failure.
  - `name`: the short, readable part name shown under the image.
  - `secondary`: small text under the name (part number, "3 / 4 left").
  - `tooltip`: the `title` text, for the details that do not fit.
  - `ariaLabel`: the button's accessible name, e.g. "Add Brick 2 x 4".
  - `onClick`: the message to send; `Nothing` disables the card.
  - `attributes`: extra attributes for the button, such as the host's
    `keepFocusOnCanvas`.

-}
type alias Config msg =
    { imageUrl : Maybe String
    , name : String
    , secondary : String
    , tooltip : String
    , ariaLabel : String
    , onClick : Maybe msg
    , attributes : List (Html.Attribute msg)
    }


{-| The `Dict` key for a part's thumbnail in a colour. The palette's
thumbnails are per colour, so the key carries both.
-}
thumbnailKey : String -> Int -> String
thumbnailKey file color =
    String.toLower file ++ ":" ++ String.fromInt color


{-| Image edge, in CSS pixels.
-}
imageSize : String
imageSize =
    "72px"


{-| Image edge of a compact card, in CSS pixels.
-}
compactImageSize : String
compactImageSize =
    "56px"


{-| Height of a dock of compact cards: the same as the camera globe panel in
Brick Contraptions (a 112 px globe inside 8 px padding and a 1 px border on each
side), so the two panels line up along the bottom of the screen.
-}
compactPanelHeight : String
compactPanelHeight =
    "130px"


{-| A count shown on a compact card's image corner, such as "3/4". `muted`
draws it dimmed (nothing of the part is left).
-}
type alias Badge =
    { text : String
    , muted : Bool
    }


{-| What a compact card shows and does.

  - `imageUrl`, `onClick`, `attributes`: as for `Config`.
  - `tooltip`: the `title` text, shown on hover; it carries the name, part number
    and colour.
  - `ariaLabel`: the button's accessible name, e.g. "Add 24T Gear (3648)".
  - `badge`: an optional count on the image corner.

-}
type alias CompactConfig msg =
    { imageUrl : Maybe String
    , tooltip : String
    , ariaLabel : String
    , badge : Maybe Badge
    , onClick : Maybe msg
    , attributes : List (Html.Attribute msg)
    }


{-| Render one part card.
-}
view : Config msg -> Html msg
view config =
    let
        enabled =
            config.onClick /= Nothing
    in
    button
        (clickAttributes config.onClick
            ++ [ Attr.disabled (not enabled)
               , Attr.class "part-card"
               , Attr.attribute "aria-label" config.ariaLabel
               , Attr.title config.tooltip
               , Attr.style "display" "flex"
               , Attr.style "flex-direction" "column"
               , Attr.style "align-items" "center"
               , Attr.style "gap" "0.25rem"
               , Attr.style "flex-shrink" "0"
               , Attr.style "width" "6.25rem"
               , Attr.style "padding" "0.375rem 0.375rem 0.5rem"
               , Attr.style "background"
                    (if enabled then
                        "rgba(255, 255, 255, 0.08)"

                     else
                        "rgba(255, 255, 255, 0.03)"
                    )
               , Attr.style "border" "1px solid rgba(255, 255, 255, 0.1)"
               , Attr.style "border-radius" "0.625rem"
               , Attr.style "color" "var(--color-text-on-dark)"
               , Attr.style "font-family" "inherit"
               , Attr.style "cursor"
                    (if enabled then
                        "pointer"

                     else
                        "not-allowed"
                    )
               , Attr.style "touch-action" "manipulation"
               , Attr.style "transition" "background 150ms ease"
               ]
            ++ config.attributes
        )
        [ backdrop enabled (thumbnail imageSize enabled config.imageUrl)
        , span
            [ Attr.style "display" "-webkit-box"
            , Attr.style "-webkit-line-clamp" "2"
            , Attr.style "-webkit-box-orient" "vertical"
            , Attr.style "overflow" "hidden"
            , Attr.style "width" "100%"
            , Attr.style "height" "2.2em"
            , Attr.style "font-size" "0.75rem"
            , Attr.style "font-weight" "600"
            , Attr.style "line-height" "1.1"
            , Attr.style "text-align" "center"
            , Attr.style "overflow-wrap" "anywhere"
            ]
            [ text config.name ]
        , span
            [ Attr.style "font-size" "0.6875rem"
            , Attr.style "line-height" "1.1"
            , Attr.style "white-space" "nowrap"
            , Attr.style "color"
                (if enabled then
                    "var(--color-text-subtle)"

                 else
                    "var(--color-brand-red)"
                )
            ]
            [ text config.secondary ]
        ]


{-| Render one compact, picture-only part card. Its name is not drawn: the
tooltip and the accessible name carry it.
-}
compactView : CompactConfig msg -> Html msg
compactView config =
    let
        enabled =
            config.onClick /= Nothing
    in
    button
        (clickAttributes config.onClick
            ++ [ Attr.disabled (not enabled)
               , Attr.class "part-card part-card-compact"
               , Attr.attribute "aria-label" config.ariaLabel
               , Attr.title config.tooltip
               , Attr.style "position" "relative"
               , Attr.style "display" "flex"
               , Attr.style "align-items" "center"
               , Attr.style "justify-content" "center"
               , Attr.style "flex-shrink" "0"
               , Attr.style "padding" "0.25rem"
               , Attr.style "background"
                    (if enabled then
                        "rgba(255, 255, 255, 0.08)"

                     else
                        "rgba(255, 255, 255, 0.03)"
                    )
               , Attr.style "border" "1px solid rgba(255, 255, 255, 0.1)"
               , Attr.style "border-radius" "0.625rem"
               , Attr.style "color" "var(--color-text-on-dark)"
               , Attr.style "font-family" "inherit"
               , Attr.style "cursor"
                    (if enabled then
                        "pointer"

                     else
                        "not-allowed"
                    )
               , Attr.style "touch-action" "manipulation"
               , Attr.style "transition" "background 150ms ease"
               ]
            ++ config.attributes
        )
        (backdrop enabled (thumbnail compactImageSize enabled config.imageUrl)
            :: Maybe.withDefault [] (Maybe.map (\badge -> [ compactBadge enabled badge ]) config.badge)
        )


{-| The count on a compact card's image corner. Decorative: the accessible name
already gives the count.
-}
compactBadge : Bool -> Badge -> Html msg
compactBadge enabled badge =
    span
        [ Attr.attribute "aria-hidden" "true"
        , Attr.style "position" "absolute"
        , Attr.style "top" "2px"
        , Attr.style "right" "2px"
        , Attr.style "padding" "0 0.3125rem"
        , Attr.style "border-radius" "9999px"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "color"
            (if badge.muted || not enabled then
                "var(--color-text-subtle)"

             else
                "var(--color-brand-yellow)"
            )
        , Attr.style "font-size" "0.625rem"
        , Attr.style "font-weight" "700"
        , Attr.style "line-height" "1.4"
        , Attr.style "white-space" "nowrap"
        , Attr.style "font-variant-numeric" "tabular-nums"
        , Attr.style "pointer-events" "none"
        ]
        [ text badge.text ]


{-| The click handler of a card, if it has one.
-}
clickAttributes : Maybe msg -> List (Html.Attribute msg)
clickAttributes onClick =
    case onClick of
        Just msg ->
            [ Html.Events.onClick msg ]

        Nothing ->
            []


{-| The light rounded tile behind the image: a light grey from the theme's
neutral tokens (`--color-border-default` towards `--color-text-subtle`), light
enough for black parts and dark enough that white parts keep an edge, with a
lighter centre so the part looks lit. Dimmed on a disabled card.
-}
backdrop : Bool -> Html msg -> Html msg
backdrop enabled child =
    div
        [ Attr.class "part-card-backdrop"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "padding" "0.25rem"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "background"
            "radial-gradient(circle at 50% 45%, var(--color-bg-subtle) 0%, color-mix(in srgb, var(--color-border-default) 80%, var(--color-text-subtle)) 75%)"
        , Attr.style "box-shadow" "inset 0 0 0 1px rgba(255, 255, 255, 0.35)"
        , Attr.style "opacity"
            (if enabled then
                "1"

             else
                "0.45"
            )
        ]
        [ child ]


{-| The rendered image at a given edge size, or a same-sized placeholder.
Decorative: the button's label already names the part.
-}
thumbnail : String -> Bool -> Maybe String -> Html msg
thumbnail size enabled maybeUrl =
    case maybeUrl of
        Just url ->
            img
                [ Attr.src url
                , Attr.alt ""
                , Attr.attribute "draggable" "false"
                , Attr.style "width" size
                , Attr.style "height" size
                , Attr.style "object-fit" "contain"
                , Attr.style "pointer-events" "none"
                , Attr.style "filter"
                    (if enabled then
                        "none"

                     else
                        "grayscale(1) brightness(0.6)"
                    )
                ]
                []

        Nothing ->
            div
                [ Attr.attribute "aria-hidden" "true"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "width" size
                , Attr.style "height" size
                , Attr.style "color" "var(--color-text-muted)"
                , Attr.style "font-size" "1.5rem"
                ]
                [ text "⬡" ]
