module UI.Theme exposing (..)

import Color exposing (Color, rgb)
import Color.Manipulate exposing (invert)
import Color.Palette exposing (fromPalette)
import Json.Decode as D exposing (Decoder)
import Parser exposing ((|.), (|=), Step(..))


type Appereance
    = Dark
    | Light


decodeColorScheme : Decoder Appereance
decodeColorScheme =
    D.string
        |> D.andThen
            (D.succeed
                << (\s ->
                        case s of
                            "light" ->
                                Light

                            "dark" ->
                                Dark

                            _ ->
                                Light
                   )
            )


getTheme : Theme -> Appereance -> ThemeType
getTheme t a =
    case a of
        Light ->
            t.light

        Dark ->
            t.dark


type alias Theme =
    { light : ThemeType
    , dark : ThemeType
    }


type ThemeType
    = Duotone Color Color


toDuotone : List Color -> ThemeType
toDuotone clrs =
    case clrs of
        fg :: bg :: [] ->
            Duotone fg bg

        fg :: [] ->
            Duotone fg (invert fg)

        _ ->
            Duotone (rgb 0.2 0.2 0.2) (rgb 1 1 1)


theme1 : Theme
theme1 =
    { light =
        fromPalette "https://coolors.co/9A63FF-FFFFFF" |> toDuotone
    , dark =
        fromPalette "https://coolors.co/6944AF-FFFFFF" |> List.reverse |> toDuotone
    }


getColors : Appereance -> ThemeType
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
