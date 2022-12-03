port module Main exposing (..)

import Browser exposing (Document)
import BrowserWindow exposing (BrowserWindow)
import Context exposing (Context, Lang(..))
import Element exposing (Element, alignBottom, centerX, centerY, el, fill, height, html, htmlAttribute, row, spacing, width)
import Element.Border
import Element.Font
import Element.Input
import Html
import Html.Attributes
import Json.Decode as D exposing (Decoder, Value)
import Math.Vector2 exposing (vec2)
import Ports exposing (PortMessage(..))
import UI exposing (col, root, text)
import UI.Color
import UI.Theme exposing (Appereance(..), decodeColorScheme)
import UI.Window exposing (viewElement)
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
                                D.map (PortMsg << UpdateBrowserWindow) (D.field "value" BrowserWindow.decode)

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
    , window : { width : Float, height : Float }
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
              , window = f.window
              }
                |> (\m -> { m | windowModel = Window.init (windowElements (getContext m) m) })
            , Cmd.none
            )
    in
    case fr of
        Ok f ->
            withF f

        Err _ ->
            withF
                { prefersColorScheme = Light
                , window =
                    { width = 1024
                    , height = 768
                    }
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
    | UpdatePrefersColorScheme Appereance
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

        UpdatePrefersColorScheme scheme ->
            ( { model | settings = model.settings |> (\s -> { s | theme = scheme }) }, Cmd.none )

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


view : Model -> Document Msg
view model =
    { title = "Binary Please"
    , body =
        let
            ctx =
                getContext model
        in
        [ root ctx
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
                (Window.view WindowMsg model.windowModel (windowElements ctx model))
            )
        ]
    }


windowElements : Context -> Model -> List ( Window, Int -> Element Msg )
windowElements ctx model =
    [ ( { position = vec2 300 300
        , size = vec2 320 240
        }
      , viewElement
            { trackWindow = WindowMsg << Window.TrackWindow, ui = ctx.ui }
            { title = text "Title"
            , content = el [ Element.Font.bold, centerX, centerY ] (text "Content")
            }
      )
    , ( { position = vec2 50 50
        , size = vec2 100 100
        }
      , viewElement
            { trackWindow = WindowMsg << Window.TrackWindow, ui = ctx.ui }
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
      )
    ]


toggleAppereanceButton : Model -> Element Msg
toggleAppereanceButton model =
    row [ spacing 8 ]
        [ Element.Input.button
            [ Element.Border.width 0
            , Element.focused
                []
            ]
            { label = fa "space-station-moon-construction"
            , onPress = Just <| ToggleAppereance Dark
            }
        , Element.Input.button
            [ Element.Border.width 0
            , Element.focused
                []
            ]
            { label =
                if model.settings.theme == Light then
                    fa "toggle-on"

                else
                    fa "toggle-off"
            , onPress =
                Just <|
                    ToggleAppereance <|
                        if model.settings.theme == Light then
                            Dark

                        else
                            Light
            }
        , Element.Input.button
            [ Element.Border.width 0
            , Element.focused
                []
            ]
            { label = fa "starship-freighter"
            , onPress = Just <| ToggleAppereance Light
            }
        ]


class : String -> Element.Attribute msg
class =
    htmlAttribute << Html.Attributes.class


fa : String -> Element msg
fa icon =
    html
        (Html.i
            [ Html.Attributes.class <| "fa-sharp fa-solid " ++ "fa-" ++ icon
            ]
            []
        )


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
    , window = m.window
    }
