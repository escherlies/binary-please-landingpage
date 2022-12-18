module UI.Window exposing (..)

import Context exposing (Context)
import Element exposing (Element, clip, column, el, fill, height, htmlAttribute, padding, px, row, scrollbars, width)
import Element.Border
import Element.Color
import Element.Font
import Html.Attributes
import UI
import Window exposing (onDrag)
import Window.Plane exposing (Plane)


viewElement : Context a -> { d | title : Element msg, content : Element msg } -> (Window.Msg -> msg) -> Int -> Plane -> Element msg
viewElement ctx { title, content } toMsg ix _ =
    column
        [ Element.Border.width 2
        , width fill
        , height fill
        , Element.Color.borderShadow
            { offset = ( 3, 3 )
            , blur = 0
            , color = ctx.ui.colors.foreground
            , size = 0
            }
        , Element.Color.backgroundColor ctx.ui.colors.background
        ]
        [ row
            ([ height (px 40)
             , width fill
             , Element.Border.widthEach
                { top = 0
                , left = 0
                , right = 0
                , bottom = 2
                }
             , onDrag toMsg ix
             , cursor "move"
             , padding 8
             , Element.Font.semiBold
             ]
                ++ userSelect False
            )
            [ title ]
        , el
            [ width fill
            , height fill
            , clip
            , scrollbars
            , padding 8
            ]
          <|
            content
        ]


cursor : String -> Element.Attribute msg
cursor =
    htmlAttribute << Html.Attributes.style "cursor"


userSelect : Bool -> List (Element.Attribute msg)
userSelect val =
    if val then
        []

    else
        [ htmlAttribute (Html.Attributes.style "user-select" "none")
        , htmlAttribute (Html.Attributes.style "-ms-user-select" "none")
        , htmlAttribute (Html.Attributes.style "-moz-user-select" "none")
        , htmlAttribute (Html.Attributes.style "-webkit-user-select" "none")
        , htmlAttribute (Html.Attributes.style "-webkit-touch-callout" "none")
        ]
