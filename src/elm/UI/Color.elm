module UI.Color exposing (..)

import Color exposing (Color)
import UI.Theme exposing (Appereance, Theme, ThemeType(..), getTheme)


type alias Colors =
    { foreground : Color
    , background : Color
    }


fromThemeType : ThemeType -> Colors
fromThemeType t =
    case t of
        Duotone fg bg ->
            { foreground = fg
            , background = bg
            }


fromTheme : Theme -> Appereance -> Colors
fromTheme t =
    fromThemeType << getTheme t
