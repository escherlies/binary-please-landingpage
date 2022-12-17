module Content exposing (..)

import Context exposing (Lang(..))
import Element exposing (alignBottom, alignTop, centerX, centerY, column, el, fill, padding, paragraph, row, spacing, width)
import Element.Font
import Math.Vector2 exposing (getX, getY, vec2)
import Ports exposing (PortMessage(..))
import UI exposing (col, fa, text)
import UI.Theme exposing (Appereance(..))
import UI.Window exposing (viewElement)
import Window exposing (Window)
import Window.Plane
import Window.Utils exposing (zero)


debugWindows : { a | ui : { b | colors : { c | foreground : Element.Color, background : Element.Color } }, trackWindow : Int -> Math.Vector2.Vec2 -> msg } -> { d | windowModel : { e | mousePosition : Math.Vector2.Vec2 }, window : Math.Vector2.Vec2 } -> List (Window msg)
debugWindows ctx model =
    [ { plane =
            { position = zero
            , size = vec2 150 200
            }
                |> Window.move (vec2 50 50)
      , render =
            \i w ->
                viewElement
                    ctx
                    { title = text <| "ix = " ++ String.fromInt i
                    , content =
                        col [ centerX, centerY ]
                            [ text <| "x = " ++ String.fromFloat (getX w.position)
                            , text <| "y = " ++ String.fromFloat (getY w.position)
                            , text <| "w = " ++ String.fromFloat (getX w.size)
                            , text <| "h = " ++ String.fromFloat (getY w.size)
                            ]
                    }
                    i
                    w
      }
    , { plane =
            { position = vec2 200 200
            , size = vec2 250 250
            }
                |> Window.move (vec2 50 50)
      , render =
            viewElement
                ctx
                { title = text <| "Mouse position, viewport size"
                , content =
                    col [ centerX, centerY ]
                        [ text <| "x = " ++ String.fromFloat (getX model.windowModel.mousePosition)
                        , text <| "y = " ++ String.fromFloat (getY model.windowModel.mousePosition)
                        , text <| "vw = " ++ String.fromFloat (getX model.window)
                        , text <| "vh = " ++ String.fromFloat (getY model.window)
                        ]
                }
      }
    ]


winddowSettings : (c -> Element.Element msg) -> { a | window : Math.Vector2.Vec2, ui : { b | colors : { d | foreground : Element.Color, background : Element.Color } }, trackWindow : e -> Math.Vector2.Vec2 -> msg } -> c -> { plane : Window.Plane.Plane, render : e -> f -> Element.Element msg }
winddowSettings toggleAppereanceButton ctx model =
    { plane =
        Window.bottomRight
            ctx.window
            { position = zero
            , size = vec2 100 100
            }
            |> Window.move (vec2 -50 -50)
    , render =
        viewElement ctx
            { title = text "Settings"
            , content =
                col [ centerX, centerY ]
                    [ el
                        [ alignBottom
                        , centerX
                        ]
                        (toggleAppereanceButton model)
                    ]
            }
    }


windowProject : { a | window : Math.Vector2.Vec2, ui : { b | colors : { c | foreground : Element.Color, background : Element.Color } }, trackWindow : d -> Math.Vector2.Vec2 -> msg } -> e -> { plane : Window.Plane.Plane, render : d -> f -> Element.Element msg }
windowProject ctx _ =
    { plane =
        Window.center
            ctx.window
            { position = zero
            , size = vec2 320 240
            }
            |> Window.centerX ctx.window
            |> Window.move (vec2 20 0)
    , render =
        viewElement ctx
            { title = text "Projects"
            , content =
                col [ centerX, centerY, width fill, padding 40 ]
                    [ Element.newTabLink
                        [ centerX ]
                        { url = "https://www.hyhyve.com/"
                        , label =
                            row [ spacing 12, width fill ]
                                [ el [ alignTop ] <| paragraph [] [ fa "up-right-from-square fa-sm" ]
                                , el [ Element.Font.bold, alignTop ] <| text "HyHyve"
                                , Element.paragraph [ alignTop ]
                                    [ text " (Online events that are fun!)"
                                    ]
                                ]
                        }
                    ]
            }
    }


windowBinaryPlease : { a | window : Math.Vector2.Vec2, ui : { b | colors : { c | foreground : Element.Color, background : Element.Color } }, trackWindow : d -> Math.Vector2.Vec2 -> msg } -> e -> { plane : Window.Plane.Plane, render : d -> f -> Element.Element msg }
windowBinaryPlease ctx _ =
    { plane =
        Window.center ctx.window
            { position = zero
            , size = vec2 330 260
            }
            |> Window.move (vec2 0 -100)
    , render =
        viewElement
            ctx
            { title = text "Binary Please UG"
            , content =
                column [ centerX, centerY ]
                    [ text "101010101010011000001010101010101011"
                    , text "101010110100110010101010101010101101"
                    , text "010101010101010101010101010100101010"
                    , text "101001010110101001010101010101010100"
                    , text "010101010101010101011010100101011001"
                    , text "010101101010101010101010101010100101"
                    , text "100101011Next1Gen11Software110101001"
                    , text "101001010101011010100110101010101010"
                    , text "101010101010011000001010101010101011"
                    , text "101000010111010111010110100010101011"
                    , text "101000101010101001011001010101010101"
                    , text "101001100110101010101101010101011011"
                    , text "011001101010100101010101010101010101"
                    ]
            }
    }


legalDisclosure : { a | window : Math.Vector2.Vec2, ui : { b | colors : { c | foreground : Element.Color, background : Element.Color } }, trackWindow : d -> Math.Vector2.Vec2 -> msg } -> e -> { plane : Window.Plane.Plane, render : d -> f -> Element.Element msg }
legalDisclosure ctx _ =
    { plane =
        Window.center ctx.window
            { position = zero
            , size = vec2 330 260
            }
            |> Window.move (vec2 0 -100)
    , render =
        viewElement
            ctx
            { title = text "Legal disclosure"
            , content =
                column [ centerX, centerY, spacing 14 ] <|
                    List.map
                        (\t ->
                            column []
                                (List.map
                                    (\t2 ->
                                        if String.startsWith "#" t2 then
                                            paragraph [ Element.Font.bold ] [ text t2 ]

                                        else
                                            paragraph
                                                [ UI.whiteSpacePreWrap ]
                                                [ text t2 ]
                                    )
                                    (String.lines t)
                                )
                        )
                    <|
                        String.split "\n\n"
                            """
# Legal disclosure

Information in accordance with Section 5 TMG

Binary Please UG (haftungsbeschränkt)
c/o Factory Works GmbH
Rheinsberger Straße 76/77
10115 Berlin

# Represented by

Enrico Scherlies (Managing Director)

# Contact Information

Telephone:        +491743812983
E-Mail:           support@hyhyve.com
Internet address: https://binaryplease.com
Register entry

Entry in:         Handelsregister
Register Number:  HRB 225 876 B
Register Court:   Amtsgericht Charlottenburg

# VAT number

VAT identification number in accordance with Section 27 an of the German VAT act DE 341 410 687

# Copyright

The contents of binaryplease.com, unless otherwise stated, is protected by copyright.
"""
            }
    }
