port module Main exposing (..)

import Browser exposing (Document)
import Context exposing (Context, Lang(..))
import Element exposing (Element, alignBottom, centerX, centerY, clip, el, fill, height, padding, width)
import Element.Font
import Json.Decode as D exposing (Decoder, Value)
import Math.Vector2 exposing (vec2)
import UI exposing (button, col, root, text)
import UI.Color
import UI.Theme exposing (Appereance(..), decodeColorScheme)
import Window exposing (Window)


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
    , windowModel : Window.Model
    }


init : D.Value -> ( Model, Cmd Msg )
init fd =
    let
        fr =
            fd
                |> D.decodeValue
                    flagsDecoder

        withF : Flags -> ( Model, Cmd msg )
        withF f =
            ( { counter = 0
              , messages = []
              , settings =
                    { theme = f.prefersColorScheme
                    }
              , windowModel = Window.empty
              }
                |> (\m -> { m | windowModel = Window.init (windowElements m) })
            , Cmd.none
            )
    in
    case fr of
        Ok f ->
            withF f

        Err _ ->
            withF { prefersColorScheme = Light }


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
    | WindowMsg Window.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowMsg sm ->
            Window.update sm model.windowModel
                |> (\( wm, wcmds ) ->
                        ( { model | windowModel = wm }, wcmds )
                   )

        CloseMessage _ ->
            Debug.todo "Implement CloseMessage"

        GotError _ ->
            Debug.todo "Implement GotError"

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
            (el
                [ width fill
                , height fill
                , clip
                , padding 20
                , Element.inFront
                    (col
                        [ width fill
                        ]
                        (List.map getMessage model.messages)
                    )
                ]
                (Window.view WindowMsg model.windowModel (windowElements model))
            )
        ]
    }


windowElements : Model -> List ( Window, Element Msg )
windowElements model =
    [ ( { position = vec2 300 300
        , size = vec2 320 240
        }
      , el [ Element.Font.bold, centerX, centerY ] (text "Content")
      )
    , ( { position = vec2 50 50
        , size = vec2 100 100
        }
      , col [ centerX, centerY ]
            [ el [ alignBottom, centerX ]
                (button
                    (if model.settings.theme == Light then
                        "☽"

                     else
                        "☼"
                    )
                    ToggleAppereance
                )
            ]
      )
    ]


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
