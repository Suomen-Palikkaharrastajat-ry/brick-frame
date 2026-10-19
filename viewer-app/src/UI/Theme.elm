module UI.Theme exposing
    ( appBackground
    , borderDefault
    , brand
    , brandRed
    , brandYellow
    , panelBackground
    , panelSubtleBackground
    , panelSurface
    , textMuted
    , textOnDark
    , textPrimary
    )

{-| Shared visual tokens for the light-first Elm UI layer.
-}


appBackground : String
appBackground =
    "var(--color-bg-page)"


panelSurface : String
panelSurface =
    "var(--color-bg-page)"


panelBackground : String
panelBackground =
    "color-mix(in srgb, var(--color-bg-page) 88%, transparent)"


panelSubtleBackground : String
panelSubtleBackground =
    "var(--color-bg-subtle)"


textPrimary : String
textPrimary =
    "var(--color-text-primary)"


textMuted : String
textMuted =
    "var(--color-text-muted)"


textOnDark : String
textOnDark =
    "var(--color-text-on-dark)"


brand : String
brand =
    "var(--color-brand)"


brandYellow : String
brandYellow =
    "var(--color-brand-yellow)"


brandRed : String
brandRed =
    "var(--color-brand-red)"


borderDefault : String
borderDefault =
    "var(--color-border-default)"
