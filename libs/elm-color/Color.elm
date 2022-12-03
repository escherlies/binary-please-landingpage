module Color exposing (..)

import Color.Internal exposing (Color(..), mapRgb)


type alias Color =
    Color.Internal.Color



-- Construct


rgb : Float -> Float -> Float -> Color
rgb =
    Color.Internal.rgb


rgba : Float -> Float -> Float -> Float -> Color
rgba =
    Color.Internal.rgba


rgb255 : Int -> Int -> Int -> Color
rgb255 =
    Color.Internal.fromRgb255


rgba255 : Int -> Int -> Int -> Int -> Color
rgba255 =
    Color.Internal.fromRgba255



-- Parse


fromHexString : String -> Maybe Color
fromHexString =
    Color.Internal.fromHexString



-- Convert


toRgba : Color -> Color.Internal.Channels
toRgba =
    Color.Internal.toRgba


toCssString : Color -> String
toCssString =
    Color.Internal.toCssString


toHexString : Color -> String
toHexString =
    Color.Internal.toHexString



-- Manipulate


invert : Color -> Color
invert =
    mapRgb (\c -> 1 - c)


setAlpha : Float -> Color -> Color
setAlpha a (Rgba r g b _) =
    Rgba r g b a
