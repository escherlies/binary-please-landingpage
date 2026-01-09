module UI.Window exposing (..)

import Context exposing (Context)
import Element exposing (Element, alignRight, clip, column, el, fill, height, htmlAttribute, padding, px, row, scrollbars, spacing, width)
import Element.Border
import Element.Color
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode as D
import UI
import Window exposing (onDrag, sendToBack)
import Window.Rect exposing (Rect)


viewElement : Context a -> { d | title : Element msg, content : Element msg } -> (Window.Msg -> msg) -> Int -> Rect -> Element msg
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
        , Element.Color.fontColor ctx.ui.colors.foreground
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
             , spacing 8
             ]
                ++ userSelect False
            )
            [ title
            , Element.Input.button
                [ alignRight
                , Element.Font.size 14
                , cursor "pointer"
                , htmlAttribute
                    (Html.Events.stopPropagationOn "pointerdown"
                        (D.succeed ( sendToBack toMsg ix, True ))
                    )
                ]
                { onPress = Nothing
                , label = UI.faEl [] "arrow-down-to-line"
                }
            ]
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
