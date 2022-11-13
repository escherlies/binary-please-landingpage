port module Main exposing (..)

import Browser
import Element exposing (el, fill, height, px, scrollbars, width)
import Element.Background
import Html exposing (Html)
import UI exposing (..)


main : Program () Model Msg
main =
    Browser.element
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { counter = 0
      , messages = []
      }
    , Cmd.none
    )


type Msg
    = Increment
    | Decrement
    | SendMessage
    | GotMessage String


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


view : Model -> Html Msg
view model =
    root
        (col center
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
                    , Element.Background.color colors.shade1
                    ]
                    (List.map text model.messages)
                ]
            ]
        )
