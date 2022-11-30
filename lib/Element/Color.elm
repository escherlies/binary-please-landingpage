module Element.Color exposing (..)

import Element



{- Interop with Elm-Ui -}
-- TODO


rgb : Float -> Float -> Float -> Element.Color
rgb =
    Element.rgb


rgba : Float -> Float -> Float -> Float -> Element.Color
rgba =
    Element.rgba


rgb255 : Int -> Int -> Int -> Element.Color
rgb255 =
    Element.rgb255


rgba255 : Int -> Int -> Int -> Float -> Element.Color
rgba255 =
    Element.rgba255


toRgb : Element.Color -> { red : Float, green : Float, blue : Float, alpha : Float }
toRgb =
    Element.toRgb


fromRgb : { red : Float, green : Float, blue : Float, alpha : Float } -> Element.Color
fromRgb =
    Element.fromRgb


fromRgb255 : { red : Int, green : Int, blue : Int, alpha : Float } -> Element.Color
fromRgb255 =
    Element.fromRgb255
