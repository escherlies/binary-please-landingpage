port module Main exposing (..)

import Browser exposing (Document)
import BrowserWindow exposing (BrowserWindow)
import Context exposing (Context, Lang(..))
import Element exposing (Element, alignBottom, alignTop, centerX, centerY, column, el, fill, height, html, htmlAttribute, padding, paragraph, row, spacing, width)
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
import Window exposing (Window, zero)


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
    [ ( Window.center ctx.window
            { position = zero
            , size = vec2 320 250
            }
      , viewElement
            { trackWindow = trackWindow, ui = ctx.ui }
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
      )
    , ( Window.bottomRight
            ctx.window
            { position = zero
            , size = vec2 100 100
            }
            |> Window.move (vec2 -50 -50)
      , viewElement
            { trackWindow = trackWindow, ui = ctx.ui }
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
    , ( Window.center
            ctx.window
            { position = zero
            , size = vec2 320 240
            }
            |> Window.centerX ctx.window
            |> Window.move (vec2 180 180)
      , viewElement
            { trackWindow = trackWindow, ui = ctx.ui }
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
      )
    ]


trackWindow : Int -> Math.Vector2.Vec2 -> Msg
trackWindow i v =
    WindowMsg (Window.TrackWindow i v)


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
