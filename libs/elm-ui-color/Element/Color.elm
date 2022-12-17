module Element.Color exposing (..)

import Color exposing (Color)
import Element
import Element.Background
import Element.Border
import Element.Font


toElementColor : Color.Color -> Element.Color
toElementColor =
    Element.fromRgb << Color.toRgba


backgroundColor : Color -> Element.Attr decorative msg
backgroundColor =
    Element.Background.color << toElementColor


fontColor : Color -> Element.Attr decorative msg
fontColor =
    Element.Font.color << toElementColor


borderShadow : { a | offset : ( Float, Float ), size : Float, blur : Float, color : Color } -> Element.Attr decorative msg
borderShadow { offset, size, blur, color } =
    Element.Border.shadow
        { offset = offset
        , size = size
        , blur = blur
        , color = toElementColor color
        }
