module Element.Color exposing (..)

import Color
import Color.Internal exposing (Color(..))
import Element


toElementColor : Color.Color -> Element.Color
toElementColor (Rgba r g b a) =
    Element.rgba r g b a
