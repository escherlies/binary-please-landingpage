port module Main exposing (..)

import Array
import Browser exposing (Document)
import Context exposing (Context, Lang(..))
import Element exposing (Element, clip, el, fill, height, htmlAttribute, padding, width)
import Element.Events exposing (onMouseUp)
import Element.Font
import Html.Events
import Json.Decode as D exposing (Decoder, Value)
import Math.Vector2 exposing (Vec2, sub, vec2)
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
    , mousePosition : Vec2
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
              , mousePosition = vec2 0 0
              , windowModel = Window.init windowElements
              }
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
    | MouseMove Float Float
    | WindowMsg Window.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowMsg sm ->
            Window.update sm model.windowModel
                |> (\( wm, wcmds ) ->
                        ( { model | windowModel = wm }, wcmds )
                   )

        MouseMove x y ->
            let
                mp =
                    vec2 x y

                delta =
                    sub mp model.mousePosition
            in
            ( { model
                | mousePosition = mp
                , windowModel = Window.updateWindows model.windowModel mp delta
              }
            , Cmd.none
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
            (col
                ([ width fill
                 , height fill
                 , clip
                 , padding 20
                 , onMouseUp (WindowMsg Window.StopTrackWindow)
                 , htmlAttribute
                    (Html.Events.on "mousemove"
                        (D.map2 MouseMove
                            (D.field "clientX" D.float)
                            (D.field "clientY" D.float)
                        )
                    )
                 , Element.inFront
                    (col
                        [ width fill
                        ]
                        (List.map getMessage model.messages)
                    )
                 ]
                    ++ renderWindows model
                )
                [ col [ Element.centerX ]
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


renderWindows : Model -> List (Element.Attribute Msg)
renderWindows model =
    let
        foo =
            List.map2
                Tuple.pair
                (Array.toList model.windowModel.windows)
                (List.map Tuple.second windowElements)
    in
    List.indexedMap Window.view foo
        |> List.map (Element.mapAttribute WindowMsg)


windowElements : List ( Window, Element msg )
windowElements =
    [ ( { position = vec2 300 300
        , size = vec2 320 240
        }
      , el [ Element.Font.bold, Element.centerX, Element.centerY ] (text "Content")
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
