module UI exposing (button, root, row, text)

import Element exposing (Element, centerX, centerY, el, height, layout, minimum, paragraph, rgb, shrink, spacing, text, width)
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
        [ Element.Font.size 18
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



-- Local


buttonWith : Element msg -> msg -> Element msg
buttonWith label msg =
    Element.Input.button
        [ Element.Border.width 1
        , Element.Border.color (rgb 0.9 0.9 0.9)
        , Element.Border.rounded 5
        , Element.Background.color (rgb 0.95 0.95 0.95)
        , width (shrink |> minimum 25)
        , height (shrink |> minimum 25)
        ]
        { onPress = Just msg
        , label = label
        }


textLabel : String -> Element msg
textLabel =
    el [ centerX, centerY ] << text
