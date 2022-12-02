port module Main exposing (..)

import Browser exposing (Document)
import Context exposing (Context, Lang(..))
import Element exposing (clip, el, fill, height, htmlAttribute, padding, px, row, width)
import Element.Border
import Element.Events exposing (onMouseDown, onMouseUp)
import Html.Events
import Json.Decode as D exposing (Decoder, Value)
import UI exposing (button, center, col, root, text)
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


type alias Position =
    { x : Float
    , y : Float
    }


setPosition : Float -> Float -> Position
setPosition x y =
    { x = x, y = y }


type alias Model =
    { counter : Int
    , messages : List Message
    , mousePosition : Position
    , windowPosition : Position
    , settings :
        { theme : Appereance
        }
    , isDragging : Bool
    }


init : D.Value -> ( Model, Cmd Msg )
init fd =
    let
        fr =
            fd
                |> D.decodeValue
                    flagsDecoder

        withF f =
            ( { counter = 0
              , messages = []
              , settings =
                    { theme = f.prefersColorScheme
                    }
              , mousePosition = { x = 0, y = 0 }
              , windowPosition = { x = 0, y = 0 }
              , isDragging = False
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
    | TrackWindow
    | StopTrackWindow
    | MouseMove Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove x y ->
            let
                mp =
                    setPosition x y

                delta =
                    { x = mp.x - model.mousePosition.x
                    , y = mp.y - model.mousePosition.y
                    }
            in
            ( { model
                | mousePosition = mp
                , windowPosition =
                    if model.isDragging then
                        { x = model.windowPosition.x + delta.x
                        , y = model.windowPosition.y + delta.y
                        }

                    else
                        model.windowPosition
              }
            , Cmd.none
            )

        TrackWindow ->
            ( { model | isDragging = True }, Cmd.none )

        StopTrackWindow ->
            ( { model | isDragging = False }, Cmd.none )

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
                [ width fill
                , height fill
                , clip
                , padding 20
                , onMouseUp StopTrackWindow
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
                , Element.inFront
                    (el
                        [ Element.moveRight model.windowPosition.x
                        , Element.moveDown model.windowPosition.y
                        ]
                     <|
                        col
                            (center
                                ++ [ Element.Border.width 3
                                   , width (px 300)
                                   , onMouseDown TrackWindow
                                   ]
                            )
                            [ row
                                [ height (px 42)
                                , width fill
                                , Element.Border.width 3
                                ]
                                []
                            , row
                                [ height (px 200)
                                , width fill
                                ]
                                []
                            ]
                    )
                ]
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
