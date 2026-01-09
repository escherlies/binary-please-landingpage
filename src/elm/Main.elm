port module Main exposing (..)

import Browser exposing (Document)
import BrowserWindow exposing (BrowserWindow)
import Content exposing (debugWindows, defaultRect, initRect, legalDisclosure, windowAbout, winddowSettings, windowBinaryPlease, windowOpenSource, windowProject)
import Context exposing (Context, Lang(..))
import Element exposing (Element, el, fill, height, row, spacing, width)
import Element.Border
import Element.Input
import Html
import Json.Decode as D exposing (Decoder, Value)
import List exposing (map)
import Math.Vector2 exposing (Vec, vec2)
import Ports exposing (PortMessage(..))
import Random
import Task
import Time
import UI exposing (col, faEl, root, text)
import UI.Color
import UI.Theme exposing (Appereance(..), decodeColorScheme)
import Utils exposing (dropRight, i_, liftResult)
import Window exposing (Window, mapRect)
import Window.Boundary exposing (Resize(..))
import Window.Rect exposing (move)


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
        , Time.every 100 GotTime
        ]


decodePortMessage : Value -> Msg
decodePortMessage =
    liftResult GotError PortMsg << Ports.decodePortMessage



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
    , random : List Int
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
              , windowModel = Window.init
              , window = f.window
              , random = []
              }
                |> (\m ->
                        { m
                            | windowModel = Window.initWith (windows (getContext m) m)
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
    | GotTime Time.Posix
    | GotRandom (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandom r ->
            ( { model
                | random =
                    if List.length model.random < (13 * 36) then
                        r ++ model.random

                    else
                        r ++ dropRight 36 model.random
              }
            , Cmd.none
            )

        GotTime _ ->
            ( model, Random.generate GotRandom (Random.list 36 (Random.int 0 1)) )

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
            ( { model | window = window }
            , msgCmd
                (WindowMsg
                    (Window.updateRects <|
                        List.map Window.toRho
                            (windows (getContext model |> (\c -> { c | window = window })) model)
                    )
                )
            )

        UpdatePrefersColorScheme scheme ->
            ( { model | settings = model.settings |> (\s -> { s | theme = scheme }) }, Cmd.none )


msgCmd : Msg -> Cmd Msg
msgCmd =
    Task.perform identity << Task.succeed


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

                .wsnw > * > .t {
                    white-space: nowrap !important;
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
                    (windows ctx model)
                )
            )
        ]
    }


spread : { a | window : Vec Float } -> List (Window msg) -> List (Window msg)
spread ctx ws =
    let
        n =
            List.length ws

        d =
            48

        xs =
            List.range 1 n
                |> map (i_ (*) d)
                |> map (i_ (-) (n * d // 2))
                |> map (i_ (*) -1)
                |> map toFloat

        ys =
            List.range 1 n
                |> map (i_ (*) d)
                |> map (i_ (-) (n * d // 2))
                |> map toFloat

        moves =
            List.map2 vec2 xs ys
    in
    -- Apply spread move to each window accordingly
    List.map2 (mapRect << move) moves (List.map (mapRect (always (defaultRect ctx))) ws)
        -- Move a little bit up because of humans
        |> List.map (mapRect (move (vec2 0 -50)))


windows : Context a -> Model -> List (Window Msg)
windows ctx model =
    spread ctx
        (map (Window initRect)
            [ legalDisclosure ctx model
            , winddowSettings toggleAppereanceButton ctx model
            , windowAbout ctx model
            , windowBinaryPlease ctx model
            , windowOpenSource ctx model
            , windowProject ctx model
            ]
            |> map ((|>) HideAnchorPoints)
        )
        ++ (if ctx.debug then
                debugWindows ctx model

            else
                []
           )


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



-- getWindowContext : Model ->  (Context {})
-- getWindowContext m =
--     { ui =
--         { colors = UI.Color.fromTheme UI.Theme.theme1 m.settings.theme
--         }
--     , lang = De
--     , version = 1
--     , window = m.window
--     , trackWindow = trackWindow
--     , debug = m.settings.debug
--     }
