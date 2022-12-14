module UI exposing (..)

import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Extra exposing (vStar)
import Html exposing (Html)
import Html.Attributes
import List exposing (singleton)
import UI.Color


type alias UI =
    { colors : UI.Color.Colors
    }


type alias UiContext a =
    { a | ui : UI }


{-| Top level node
-}
root : UiContext a -> Element msg -> Html msg
root ctx =
    layout
        [ fontSizeNormal
        , Element.Background.color ctx.ui.colors.background
        , Element.Font.color ctx.ui.colors.foreground
        , width fill
        , height fill
        , Element.Font.family
            [ Element.Font.monospace
            ]
        ]



-- Exposed ui elements


button : String -> msg -> Element msg
button =
    buttonWith << textLabel


{-| Default text element

Wrap text in a paragraph by default, since text will not break words.

-}
text : String -> Element msg
text =
    paragraph [] << singleton << Element.text


row : List (Element msg) -> Element msg
row =
    Element.row [ centerX, centerY, spacing 8 ]


col : List (Element.Attribute msg) -> List (Element msg) -> Element msg
col =
    Element.column << List.append [ spacing 8 ]


center : List (Element.Attribute msg)
center =
    [ centerX, centerY ]



-- Local


colors : { shade1 : Color, shade2 : Color, shade3 : Color }
colors =
    { shade1 = rgb 0.98 0.98 0.98
    , shade2 = rgb 0.95 0.95 0.95
    , shade3 = rgb 0.9 0.9 0.9
    }


buttonWith : Element msg -> msg -> Element msg
buttonWith label msg =
    Element.Input.button
        [ Element.Border.width 3
        , width (shrink |> minimum 25)
        , paddingXY 14 11
        ]
        { onPress = Just msg
        , label = label
        }


textLabel : String -> Element msg
textLabel =
    el [ centerX, centerY ] << Element.text



-- Font Sizer


fontSizer : Int -> Attr decorative msg
fontSizer =
    Element.Font.size << round << modular 14 1.25


fontSizeSm : Attr decorative msg
fontSizeSm =
    fontSizer -1


fontSizeNormal : Attr decorative msg
fontSizeNormal =
    fontSizer 1


class : String -> Attribute msg
class =
    htmlAttribute << Html.Attributes.class


whiteSpacePreWrap : Attribute msg
whiteSpacePreWrap =
    class "wspw"


faEl : List (Element.Attribute msg) -> String -> Element msg
faEl =
    vStar el fa


fa : String -> Element msg
fa icon =
    html
        (Html.i
            [ Html.Attributes.class <| "fa-sharp fa-solid " ++ "fa-" ++ icon
            ]
            []
        )
