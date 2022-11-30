module UI.Theme exposing (..)

import Color exposing (Color, rgb)
import Parser exposing ((|.), (|=), Step(..))


type Appereance
    = Dark
    | Light


type Theme
    = Duotone Color Color


getColors : Appereance -> Theme
getColors theme =
    case theme of
        Light ->
            Duotone
                (rgb 0.2 0.2 0.2)
                (rgb 1 1 1)

        Dark ->
            Duotone
                (rgb 1 1 1)
                (rgb 0.2 0.2 0.2)
