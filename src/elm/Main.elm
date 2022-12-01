port module Main exposing (..)

import Browser exposing (Document)
import Context exposing (Context, Lang(..))
import Element exposing (el, fill, height, padding, px, scrollbars, width)
import UI exposing (..)
import UI.Color
import UI.Theme exposing (Appereance(..))


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ portReceive GotMessage
        ]



-- PORTS


port portSend : String -> Cmd msg


port portReceive : (String -> msg) -> Sub msg



-- Model


type alias Model =
    { counter : Int
    , messages : List String
    , settings :
        { theme : Appereance
        }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { counter = 0
      , messages = []
      , settings =
            { theme = Light
            }
      }
    , Cmd.none
    )


type Msg
    = Increment
    | Decrement
    | SendMessage
    | GotMessage String
    | ToggleAppereance


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        SendMessage ->
            ( model, portSend "Hello!" )

        GotMessage m ->
            ( { model | messages = m :: model.messages }, Cmd.none )

        ToggleAppereance ->
            (case model.settings.theme of
                Light ->
                    Dark

                Dark ->
                    Light
            )
                |> (\new -> ( { model | settings = model.settings |> (\s -> { s | theme = new }) }, Cmd.none ))


view : Model -> Document Msg
view model =
    { title = "Binary Please"
    , body =
        [ root (getContext model)
            (col [ width fill, height fill, padding 20 ]
                [ col center
                    [ row
                        [ button "-" Decrement
                        , el [ Element.centerX ] <| text (String.fromInt model.counter)
                        , button "+" Increment
                        ]
                    , col []
                        [ button "Send Message" SendMessage
                        , col
                            [ height (px 100)
                            , scrollbars
                            , width fill

                            -- , Element.Background.color colors.shade1
                            ]
                            (List.map text model.messages)
                        ]
                    ]
                , col [ Element.centerX ]
                    [ el [ Element.alignBottom, Element.centerX ]
                        (button
                            (if model.settings.theme == Light then
                                "☽"

                             else
                                "☼"
                            )
                            ToggleAppereance
                        )
                    ]
                ]
            )
        ]
    }


getContext : Model -> Context
getContext m =
    { ui =
        { colors = UI.Color.fromTheme UI.Theme.fromPalettes m.settings.theme
        }
    , lang = De
    , version = 1
    }
