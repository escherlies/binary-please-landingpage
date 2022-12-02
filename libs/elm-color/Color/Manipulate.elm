module Color.Manipulate exposing (..)

import Color.Internal exposing (Color, mapRgb)


invert : Color -> Color
invert =
    mapRgb (\c -> 1 - c)
