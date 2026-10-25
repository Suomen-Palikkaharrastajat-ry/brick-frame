module Studio.KeyedText exposing (view)

{-| Short text that changes often (a count, a speed, a selection summary),
keyed by its value.

A browser translating the page rewrites the text node it finds. A keyed span
gets a new element whenever the value changes, so a translated copy is not left
behind next to the new text. Same technique as `UI.PartCard`, which does not
export its helper.

-}

import Html exposing (Html, span, text)
import Html.Keyed as Keyed


{-| Render `value` as a keyed span, so a changed value is a new element.
-}
view : String -> Html msg
view value =
    Keyed.node "span" [] [ ( value, span [] [ text value ] ) ]
