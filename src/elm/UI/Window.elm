module UI.Window exposing (..)

import Context exposing (Context)
import Element exposing (Element, clip, column, el, fill, height, htmlAttribute, padding, px, row, scrollbars, width)
import Element.Background
import Element.Border
import Element.Font
import Html.Events
import Json.Decode as D
import Math.Vector2 exposing (vec2)
import UI
import Window exposing (cursor, userSelect)


type alias WindowElement msg =
    { title : Element msg, content : Element msg }


viewElement : Context a -> { d | title : Element msg, content : Element msg } -> (e -> Math.Vector2.Vec2 -> msg) -> e -> f -> Element msg
viewElement ctx { title, content } trackWindow ix _ =
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
             , htmlAttribute
                (Html.Events.on "pointerdown"
                    (D.map (trackWindow ix)
                        (D.map2 vec2
                            (D.field "clientX" D.float)
                            (D.field "clientY" D.float)
                        )
                    )
                )
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
