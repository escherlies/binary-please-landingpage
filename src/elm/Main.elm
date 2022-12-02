port module Main exposing (..)

import Array exposing (Array)
import Browser exposing (Document)
import Context exposing (Context, Lang(..))
import Element exposing (Attribute, Element, clip, el, fill, height, htmlAttribute, moveDown, moveLeft, moveRight, moveUp, padding, px, rgb, row, width)
import Element.Background
import Element.Border
import Element.Events exposing (onMouseDown, onMouseUp)
import Element.Font
import Html.Attributes
import Html.Events
import Json.Decode as D exposing (Decoder, Value)
import Math.Vector2 exposing (Vec2, add, getX, getY, scale, sub, vec2)
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


type Corner
    = Top
    | Right
    | Bottom
    | Left
    | BottomRight
    | BottomLeft
    | TopRight
    | TopLeft


type Dragging
    = None
    | Reszie Int Corner
    | Move Int


type alias Model =
    { counter : Int
    , messages : List Message
    , mousePosition : Vec2
    , windows : Array Window
    , settings :
        { theme : Appereance
        }
    , isDragging : Dragging
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
              , windows = Array.fromList <| List.map Tuple.first windowElements
              , isDragging = None
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
    | ResizeWindow Int Corner
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
                    case model.isDragging of
                        None ->
                            model.windows

                        Move ix ->
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

                        Reszie ix corner ->
                            let
                                cw =
                                    Array.get ix model.windows
                            in
                            case cw of
                                Just wp ->
                                    handleRezise ix wp corner delta model.windows

                                Nothing ->
                                    -- Should never happen
                                    model.windows
              }
            , Cmd.none
            )

        ResizeWindow ix dir ->
            ( { model | isDragging = Reszie ix dir }, Cmd.none )

        TrackWindow ix ->
            ( { model | isDragging = Move ix }, Cmd.none )

        StopTrackWindow ->
            ( { model | isDragging = None }, Cmd.none )

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


handleRezise : Int -> Window -> Corner -> Vec2 -> Array Window -> Array Window
handleRezise ix wp corner delta windows =
    case corner of
        Bottom ->
            Array.set ix
                { wp | size = add wp.size (Math.Vector2.setX 0 delta) }
                windows

        Top ->
            Array.set ix
                { wp
                    | size = add wp.size (Math.Vector2.setX 0 delta |> scale -1)
                    , position = add wp.position (Math.Vector2.setX 0 delta)
                }
                windows

        Right ->
            Array.set ix
                { wp | size = add wp.size (Math.Vector2.setY 0 delta) }
                windows

        Left ->
            Array.set ix
                { wp
                    | size = add wp.size (Math.Vector2.setY 0 delta |> scale -1)
                    , position = add wp.position (Math.Vector2.setY 0 delta)
                }
                windows

        BottomRight ->
            Array.set ix
                { wp | size = add wp.size delta }
                windows

        BottomLeft ->
            Array.set ix
                { wp
                    | size =
                        add wp.size (Math.Vector2.setY 0 delta |> scale -1)
                            |> (\size -> add size (Math.Vector2.setX 0 delta))
                    , position = add wp.position (Math.Vector2.setY 0 delta)
                }
                windows

        TopRight ->
            Array.set ix
                { wp
                    | size =
                        add wp.size (Math.Vector2.setX 0 delta |> scale -1)
                            |> (\size -> add size (Math.Vector2.setY 0 delta))
                    , position = add wp.position (Math.Vector2.setX 0 delta)
                }
                windows

        TopLeft ->
            Array.set ix
                { wp
                    | size = add wp.size (delta |> scale -1)
                    , position = add wp.position delta
                }
                windows


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
            List.map2 Tuple.pair (Array.toList model.windows) (List.map Tuple.second windowElements)
    in
    List.indexedMap rendewWindow foo


bc : Corner -> List (Attribute Msg) -> String -> Int -> Element Msg
bc corner attrs c ix =
    el
        ([ onMouseDown (ResizeWindow ix corner)
         , cursor c
         ]
            ++ attrs
        )
        Element.none


rendewWindow : Int -> ( Window, Element Msg ) -> Element.Attribute Msg
rendewWindow ix ( position, content ) =
    let
        bw =
            3

        overhang =
            0

        rs =
            bw
    in
    Element.inFront
        (el
            [ Element.moveRight (getX position.position)
            , Element.moveDown (getY position.position)
            , height (px <| round <| getY position.size)
            , width (px <| round <| getX position.size)
            , Element.Background.color (rgb 0.98 0.98 0.98)
            , Element.onLeft
                (bc Left
                    [ height fill
                    , width (px rs)
                    , moveRight rs
                    ]
                    "ew-resize"
                    ix
                )
            , Element.onRight
                (bc Right
                    [ height fill
                    , width (px rs)
                    , moveLeft rs
                    ]
                    "ew-resize"
                    ix
                )
            , Element.above
                (row [ width fill, moveDown (bw + overhang) ]
                    [ bc TopLeft [ height (px rs), width (px rs) ] "nw-resize" ix
                    , bc Top [ height (px rs), width fill ] "ns-resize" ix
                    , bc TopRight [ height (px rs), width (px rs) ] "ne-resize" ix
                    ]
                )
            , Element.below
                (row [ width fill, moveUp (bw + overhang) ]
                    [ bc BottomLeft [ height (px rs), width (px rs) ] "sw-resize" ix
                    , bc Bottom [ height (px rs), width fill ] "ns-resize" ix
                    , bc BottomRight [ height (px rs), width (px rs) ] "se-resize" ix
                    ]
                )
            ]
         <|
            col
                [ Element.Border.width 3
                , width fill
                , height fill
                ]
                [ row
                    [ height (px 40)
                    , width fill
                    , Element.Border.widthEach
                        { top = 0
                        , left = 0
                        , right = 0
                        , bottom = 3
                        }
                    , onMouseDown (TrackWindow ix)
                    ]
                    []
                , content
                ]
        )


cursor : String -> Attribute msg
cursor c =
    htmlAttribute (Html.Attributes.style "cursor" c)


resizer : Corner -> (Element Msg -> a) -> List (Attribute Msg) -> String -> Int -> a
resizer corner position adjustPosition c ix =
    position
        (el
            (adjustPosition
                ++ [ Element.mouseOver
                        [ Element.Background.color (rgb 1 0.5 0.5)
                        ]
                   , htmlAttribute (Html.Attributes.style "cursor" c)
                   , onMouseDown (ResizeWindow ix corner)
                   ]
            )
            Element.none
        )


windowElements : List ( Window, Element Msg )
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
