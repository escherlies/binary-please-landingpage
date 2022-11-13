module UI exposing (button, center, col, colors, root, row, text)

import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import List exposing (singleton)


{-| Top level node
-}
root : Element msg -> Html msg
root =
    layout
        [ Element.Font.size 14
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
        [ Element.Border.width 1
        , Element.Border.color colors.shade3
        , Element.Border.rounded 5
        , Element.Background.color colors.shade2
        , width (shrink |> minimum 25)
        , paddingXY 14 11
        ]
        { onPress = Just msg
        , label = label
        }


textLabel : String -> Element msg
textLabel =
    el [ centerX, centerY ] << Element.text
