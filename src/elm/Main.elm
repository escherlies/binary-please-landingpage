port module Main exposing (..)

import Browser exposing (Document)
import Context exposing (Context, Lang(..))
import Element exposing (el, fill, height, padding, width)
import Json.Decode as D exposing (Decoder, Value)
import UI exposing (..)
import UI.Color
import UI.Theme exposing (Appereance(..), decodeColorScheme)


type alias Flags =
    { prefersColorScheme : Appereance
    }


main : Program D.Value Model Msg
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
        [ portReceive decodePortMessage
        ]


decodePortMessage : Value -> Msg
decodePortMessage dv =
    dv
        |> D.decodeValue
            (D.field "tag" D.string
                |> D.andThen
                    (\tag ->
                        case tag of
                            "UpdatePrefersColorScheme" ->
                                D.map UpdatePrefersColorScheme (D.field "value" decodeColorScheme)

                            other ->
                                D.fail <| "Unkowm message type" ++ other
                    )
            )
        |> Result.mapError D.errorToString
        |> (\r ->
                case r of
                    Ok msg ->
                        msg

                    Err s ->
                        GotError s
           )



-- PORTS


port portSend : String -> Cmd msg


port portReceive : (D.Value -> msg) -> Sub msg



-- Model


type Message
    = Error String


type alias Model =
    { counter : Int
    , messages : List Message
    , settings :
        { theme : Appereance
        }
    }


init : D.Value -> ( Model, Cmd Msg )
init fd =
    let
        fr =
            fd
                |> D.decodeValue
                    flagsDecoder
    in
    case fr of
        Ok f ->
            ( { counter = 0
              , messages = []
              , settings =
                    { theme = f.prefersColorScheme
                    }
              }
            , Cmd.none
            )

        Err _ ->
            ( { counter = 0
              , messages = []
              , settings =
                    { theme = Light
                    }
              }
            , Cmd.none
            )


flagsDecoder : Decoder Flags
flagsDecoder =
    D.map Flags
        (D.field "prefersColorScheme" decodeColorScheme)


type Msg
    = Increment
    | Decrement
    | SendMessage
    | ToggleAppereance
    | UpdatePrefersColorScheme Appereance
    | GotError String
    | CloseMessage Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseMessage index ->
            ( model, Cmd.none )

        GotError s ->
            ( model, Cmd.none )

        UpdatePrefersColorScheme scheme ->
            ( { model | settings = model.settings |> (\s -> { s | theme = scheme }) }, Cmd.none )

        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        SendMessage ->
            ( model, portSend "Hello!" )

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
            (col
                [ width fill
                , height fill
                , padding 20
                , Element.inFront
                    (col
                        [ width fill
                        ]
                        (List.map getMessage model.messages)
                    )
                ]
                [ col center
                    [ row
                        [ button "-" Decrement
                        , el [ Element.centerX ] <| text (String.fromInt model.counter)
                        , button "+" Increment
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


getMessage : Message -> Element.Element Msg
getMessage arg1 =
    case arg1 of
        Error err ->
            el [] (text err)


getContext : Model -> Context
getContext m =
    { ui =
        { colors = UI.Color.fromTheme UI.Theme.theme1 m.settings.theme
        }
    , lang = De
    , version = 1
    }
