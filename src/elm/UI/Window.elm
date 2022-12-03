module UI.Window exposing (..)

import Element exposing (Element, clip, column, el, fill, height, padding, px, row, scrollbars, width)
import Element.Background
import Element.Border
import Element.Events exposing (onMouseDown)
import Element.Font
import UI exposing (UI)
import Window exposing (cursor, userSelect)


type alias WindowElement msg =
    { title : Element msg, content : Element msg }


viewElement : { a | trackWindow : Int -> msg, ui : UI } -> WindowElement msg -> Int -> Element msg
viewElement ctx { title, content } ix =
    column
        [ Element.Border.width 2
        , width fill
        , height fill
        , Element.Border.shadow
            { offset = ( 3, 3 )
            , blur = 0
            , color = ctx.ui.colors.foreground
            , size = 0
            }
        , Element.Background.color ctx.ui.colors.background
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
             , onMouseDown (ctx.trackWindow ix)
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
            ]
          <|
            content
        ]
