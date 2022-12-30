module Content exposing (..)

import BrowserWindow exposing (BrowserWindow)
import Context exposing (Context, Lang(..))
import Element exposing (alignBottom, alignTop, centerX, centerY, column, el, fill, padding, paragraph, row, spacing, width)
import Element.Font
import List exposing (foldl)
import List.Extra
import Math.Vector2 exposing (Vec, getX, getY, vec2)
import Ports exposing (PortMessage(..))
import UI exposing (col, fa, text)
import UI.Theme exposing (Appereance(..))
import UI.Window exposing (viewElement)
import Window exposing (Window)
import Window.Rect
import Window.Utils exposing (zero)


initRect : Window.Rect.Rect
initRect =
    { position = zero, size = vec2 330 260 }


defaultRect : { a | window : Vec Float } -> Window.Rect.Rect
defaultRect ctx =
    initRect
        |> Window.Rect.center ctx.window



-- Misc


debugWindows : Context a -> { b | windowModel : { c | mousePosition : Vec Float }, window : Vec Float } -> List (Window msg)
debugWindows ctx model =
    [ { rect = defaultRect ctx
      , render =
            \tw i w ->
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
                    tw
                    i
                    w
      }
    , { rect = defaultRect ctx
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


winddowSettings : (c -> Element.Element msg) -> Context a -> c -> (Window.Msg -> msg) -> Int -> Window.Rect.Rect -> Element.Element msg
winddowSettings toggleAppereanceButton ctx model =
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



-- Projects


projectWithDescriptionBelow : { a | url : String, title : String, description : String } -> Element.Element msg
projectWithDescriptionBelow { url, title, description } =
    Element.newTabLink
        []
        { url = url
        , label =
            row [ spacing 12, width fill ]
                [ el [ alignTop ] <| paragraph [] [ fa "up-right-from-square fa-sm" ]
                , column [ width fill, spacing 8 ]
                    [ el [ Element.Font.bold, alignTop, UI.whiteSpaceNoWrap ] <| text title
                    , Element.paragraph [ alignTop ]
                        [ text description
                        ]
                    ]
                ]
        }


windowProject : { a | version : Int, lang : Lang, ui : UI.UI, window : BrowserWindow, debug : Bool } -> b -> (Window.Msg -> msg) -> Int -> Window.Rect.Rect -> Element.Element msg
windowProject ctx _ =
    viewElement ctx
        { title = text "Projects"
        , content =
            col [ centerX, centerY, width fill, padding 20, spacing 20 ] <|
                List.map projectWithDescriptionBelow
                    [ { url = "https://www.hyhyve.com/"
                      , title = "HyHyve"
                      , description = "Online events that are fun!"
                      }
                    , { url = "https://www.coffeechat.ai/"
                      , title = "CoffeeChat.ai"
                      , description = "Facilitate intra-company communications with scheduled meetings."
                      }
                    ]
        }


windowOpenSource : { a | version : Int, lang : Lang, ui : UI.UI, window : BrowserWindow, debug : Bool } -> b -> (Window.Msg -> msg) -> Int -> Window.Rect.Rect -> Element.Element msg
windowOpenSource ctx _ =
    viewElement ctx
        { title = text "Open soure"
        , content =
            col [ centerX, centerY, width fill, padding 20, spacing 20 ]
                [ text "Packages"
                , col [ centerX, centerY, width fill, padding 10, spacing 20 ] <|
                    List.map projectWithDescriptionBelow
                        [ { url = "https://github.com/escherlies/elm-color"
                          , title = "elm-color"
                          , description = "An Elm package to work with web colors."
                          }
                        , { url = "https://github.com/escherlies/elm-ui-window"
                          , title = "elm-ui-window"
                          , description = "The engine that powers this page."
                          }
                        , { url = "https://github.com/escherlies/elm-ix-dict"
                          , title = "elm-ix-dict"
                          , description = "A Dict data structure that derives keys from values."
                          }
                        ]
                , text "Tools"
                , col [ centerX, centerY, width fill, padding 10, spacing 20 ] <|
                    List.map projectWithDescriptionBelow
                        [ { url = "https://github.com/escherlies/bumpi"
                          , title = "bumpi"
                          , description = "Bumpi - a get next semantic version bump cli tool"
                          }
                        , { url = "https://github.com/escherlies/elm-generate-readme"
                          , title = "elm-generate-readme"
                          , description = "Generate a README.md from an Elm file."
                          }
                        ]
                , text "Misc"
                , col [ centerX, centerY, width fill, padding 10, spacing 20 ] <|
                    List.map projectWithDescriptionBelow
                        [ { url = "https://github.com/escherlies/binary-please-landingpage"
                          , title = "binary-please-landingpage"
                          , description = "This page."
                          }
                        ]
                ]
        }



-- Fun


windowBinaryPlease : { a | version : Int, lang : Lang, ui : UI.UI, window : BrowserWindow, debug : Bool } -> { b | random : List Int } -> (Window.Msg -> msg) -> Int -> Window.Rect.Rect -> Element.Element msg
windowBinaryPlease ctx model =
    viewElement
        ctx
        { title = text "Binary Please"
        , content =
            column [ centerX ] <|
                (model.random
                    |> List.map String.fromInt
                    |> List.Extra.groupsOf 36
                    |> List.map String.concat
                    |> mapAt 4 (replaceAtCenter "_Build with_")
                    |> mapAt 6 (replaceAtCenter "_Purely functional programming_")
                    |> mapAt 8 (replaceAtCenter "_<3_")
                    |> List.map text
                )
        }



-- Boring stuff


legalDisclosure : { a | version : Int, lang : Lang, ui : UI.UI, window : BrowserWindow, debug : Bool } -> b -> (Window.Msg -> msg) -> Int -> Window.Rect.Rect -> Element.Element msg
legalDisclosure ctx _ =
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



-- Utils


mapAt : Int -> (a -> a) -> List a -> List a
mapAt ix fn lst =
    List.Extra.getAt ix lst
        |> Maybe.map fn
        |> Maybe.map (\v -> List.Extra.setAt ix v lst)
        |> Maybe.withDefault lst


replaceAtCenter : String -> String -> String
replaceAtCenter source target =
    let
        targetLength =
            String.length target

        sourceLength =
            String.length source
    in
    replaceAt (targetLength // 2 - sourceLength // 2) source target


replaceAt : Int -> String -> String -> String
replaceAt ix source target =
    let
        words =
            String.words source
    in
    foldl
        (\word ( offset, acc ) ->
            ( offset + String.length word + 1, replaceWordAt offset word acc )
        )
        ( ix, target )
        words
        |> Tuple.second


replaceWordAt : Int -> String -> String -> String
replaceWordAt ix source target =
    String.slice 0 ix target ++ source ++ String.slice (ix + String.length source) (String.length target) target
