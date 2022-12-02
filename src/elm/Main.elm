port module Main exposing (..)

import Array exposing (Array)
import Browser exposing (Document)
import Context exposing (Context, Lang(..))
import Element exposing (Element, clip, el, fill, height, htmlAttribute, padding, px, row, width)
import Element.Border
import Element.Events exposing (onMouseDown, onMouseUp)
import Html.Events
import Json.Decode as D exposing (Decoder, Value)
import Math.Vector2 exposing (Vec2, add, getX, getY, sub, vec2)
import Maybe.Extra
import UI exposing (button, col, root, text)
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


type alias Window =
    { position : Vec2
    , size : Vec2
    }


type alias Model =
    { counter : Int
    , messages : List Message
    , mousePosition : Vec2
    , windows : Array Window
    , settings :
        { theme : Appereance
        }
    , isDragging : Maybe Int
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
              , windows = Array.fromList <| List.map Tuple.first windows
              , isDragging = Nothing
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
    | TrackWindow Int
    | StopTrackWindow
    | MouseMove Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove x y ->
            let
                mp =
                    vec2 x y

                delta =
                    sub mp model.mousePosition
            in
            ( { model
                | mousePosition = mp
                , windows =
                    Maybe.Extra.unwrap
                        model.windows
                        (\ix ->
                            let
                                cw =
                                    Array.get ix model.windows
                            in
                            case cw of
                                Just wp ->
                                    Array.set ix
                                        { wp | position = add wp.position delta }
                                        model.windows

                                Nothing ->
                                    -- Should never happen
                                    model.windows
                        )
                        model.isDragging
              }
            , Cmd.none
            )

        TrackWindow ix ->
            ( { model | isDragging = Just ix }, Cmd.none )

        StopTrackWindow ->
            ( { model | isDragging = Nothing }, Cmd.none )

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
            List.map2 Tuple.pair (Array.toList model.windows) (List.map Tuple.second windows)
    in
    List.indexedMap rendewWindow foo


rendewWindow : Int -> ( Window, Element Msg ) -> Element.Attribute Msg
rendewWindow ix ( position, content ) =
    Element.inFront
        (el
            [ Element.moveRight (getX position.position)
            , Element.moveDown (getY position.position)
            , height (px <| round <| getY position.size)
            , width (px <| round <| getX position.size)
            ]
         <|
            col
                [ Element.Border.width 3
                , width fill
                , height fill
                ]
                [ row
                    [ height (px 42)
                    , width fill
                    , Element.Border.width 3
                    , onMouseDown (TrackWindow ix)
                    ]
                    []
                , row
                    [ height (px 200)
                    , width fill
                    ]
                    [ content ]
                ]
        )


windows : List ( Window, Element Msg )
windows =
    [ ( { position = vec2 300 300
        , size = vec2 320 240
        }
      , text "Content"
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
