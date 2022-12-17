port module Main exposing (..)

import Browser exposing (Document)
import BrowserWindow exposing (BrowserWindow)
import Content exposing (debugWindows, legalDisclosure, winddowSettings, windowBinaryPlease, windowProject)
import Context exposing (Context, Lang(..))
import Element exposing (Element, el, fill, height, row, spacing, width)
import Element.Border
import Element.Input
import Html
import Json.Decode as D exposing (Decoder, Value)
import Math.Vector2 exposing (vec2)
import Ports exposing (PortMessage(..))
import UI exposing (col, faEl, root, text)
import UI.Color
import UI.Theme exposing (Appereance(..), decodeColorScheme)
import Window exposing (Window)


type alias Flags =
    { prefersColorScheme : Appereance
    , window : BrowserWindow
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

                            "UpdateBrowserWindow" ->
                                D.map UpdateBrowserWindow (D.field "value" BrowserWindow.decode)

                            other ->
                                D.fail <| "Unkowm message type" ++ other
                    )
            )
        |> Result.mapError D.errorToString
        |> (\r ->
                case r of
                    Ok msg ->
                        PortMsg msg

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
        , debug : Bool
        }
    , windowModel : Window.Model
    , window : BrowserWindow
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
                    , debug = False
                    }
              , windowModel = Window.empty
              , window = f.window
              }
                |> (\m ->
                        { m
                            | windowModel =
                                Window.initWith (windowElements (getWindowContext m) m)
                        }
                   )
            , Cmd.none
            )
    in
    case fr of
        Ok f ->
            withF f

        Err _ ->
            withF
                { prefersColorScheme = Light
                , window = vec2 1024 768
                }


flagsDecoder : Decoder Flags
flagsDecoder =
    D.map2 Flags
        (D.field "prefersColorScheme" decodeColorScheme)
        (D.field "window" BrowserWindow.decode)


type Msg
    = Increment
    | Decrement
    | SendMessage
    | ToggleAppereance Appereance
    | GotError String
    | WindowMsg Window.Msg
    | PortMsg Ports.PortMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PortMsg pm ->
            handlePortMessages pm model

        WindowMsg sm ->
            Window.update sm model.windowModel
                |> (\( wm, wcmds ) ->
                        ( { model | windowModel = wm }, wcmds )
                   )

        GotError _ ->
            ( model, Cmd.none )

        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        SendMessage ->
            ( model, portSend "Hello!" )

        ToggleAppereance t ->
            ( { model | settings = model.settings |> (\s -> { s | theme = t }) }, Cmd.none )


handlePortMessages : PortMessage -> Model -> ( Model, Cmd Msg )
handlePortMessages pm model =
    case pm of
        UpdateBrowserWindow window ->
            ( { model | window = window }, Cmd.none )

        UpdatePrefersColorScheme scheme ->
            ( { model | settings = model.settings |> (\s -> { s | theme = scheme }) }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Binary Please"
    , body =
        let
            ctx =
                getContext model
        in
        [ Html.node "style"
            []
            [ Html.text """
                .wspw > * > .t {
                    white-space: pre-wrap !important;
                }
                """
            ]
        , root ctx
            (el
                [ width fill
                , height fill
                , Element.inFront
                    (col
                        [ width fill
                        ]
                        (List.map getMessage model.messages)
                    )
                ]
                (Window.view
                    WindowMsg
                    model.windowModel
                    (windowElements (getWindowContext model) model)
                )
            )
        ]
    }


type alias WithTrackWindow a =
    { a
        | trackWindow : Int -> Math.Vector2.Vec2 -> Msg
    }


windowElements : Context (WithTrackWindow a) -> Model -> List (Window Msg)
windowElements ctx model =
    [ legalDisclosure ctx model
    , windowBinaryPlease ctx model
    , windowProject ctx model
    , winddowSettings toggleAppereanceButton ctx model
    ]
        ++ (if ctx.debug then
                debugWindows ctx model

            else
                []
           )


trackWindow : Int -> Math.Vector2.Vec2 -> Msg
trackWindow i v =
    WindowMsg (Window.TrackWindow (Window.Index i) v)


toggleAppereanceButton : { a | settings : { b | theme : Appereance } } -> Element Msg
toggleAppereanceButton model =
    Element.Input.button
        [ Element.Border.width 0
        , Element.focused []
        ]
        { label =
            row [ spacing 6 ]
                [ faEl [] "space-station-moon-construction"
                , if model.settings.theme == Light then
                    faEl [] "toggle-on"

                  else
                    faEl [] "toggle-off"
                , faEl [] "starship-freighter"
                ]
        , onPress =
            Just <|
                ToggleAppereance <|
                    if model.settings.theme == Light then
                        Dark

                    else
                        Light
        }


getMessage : Message -> Element.Element Msg
getMessage arg1 =
    case arg1 of
        Error err ->
            el [] (text err)


getContext : Model -> Context {}
getContext m =
    { ui =
        { colors = UI.Color.fromTheme UI.Theme.theme1 m.settings.theme
        }
    , lang = De
    , version = 1
    , window = m.window
    , debug = m.settings.debug
    }


getWindowContext : Model -> WithTrackWindow (Context {})
getWindowContext m =
    { ui =
        { colors = UI.Color.fromTheme UI.Theme.theme1 m.settings.theme
        }
    , lang = De
    , version = 1
    , window = m.window
    , trackWindow = trackWindow
    , debug = m.settings.debug
    }
