module UI.Color exposing (..)

import Element exposing (Color)
import Element.Color exposing (toElementColor)
import UI.Theme exposing (Appereance, Theme, ThemeType(..), getTheme)


type alias Colors =
    { foreground : Color
    , background : Color
    }


fromThemeType : ThemeType -> Colors
fromThemeType t =
    case t of
        Duotone fg bg ->
            { foreground = toElementColor fg
            , background = toElementColor bg
            }


fromTheme : Theme -> Appereance -> Colors
fromTheme t =
    fromThemeType << getTheme t
