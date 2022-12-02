module Window exposing (..)

import Array exposing (Array)
import Element exposing (Attribute, Element, clip, column, el, fill, height, htmlAttribute, moveDown, moveLeft, moveRight, moveUp, px, row, width)
import Element.Border
import Element.Events exposing (onMouseDown, onMouseUp)
import Html.Attributes
import Html.Events
import Json.Decode as D
import Math.Vector2 exposing (Vec2, add, getX, getY, scale, sub, vec2)


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
    { windows : Array Window
    , isDragging : Dragging
    , mousePosition : Vec2
    }


init : List ( a, b ) -> { windows : Array a, isDragging : Dragging, mousePosition : Vec2 }
init windowElements =
    { windows = Array.fromList <| List.map Tuple.first windowElements
    , isDragging = None
    , mousePosition = vec2 0 0
    }


empty : Model
empty =
    { windows = Array.empty
    , isDragging = None
    , mousePosition = vec2 0 0
    }


type Msg
    = TrackWindow Int
    | ResizeWindow Int Corner
    | StopTrackWindow
    | MouseMove Float Float


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ResizeWindow ix dir ->
            ( { model | isDragging = Reszie ix dir }, Cmd.none )

        TrackWindow ix ->
            ( { model | isDragging = Move ix }, Cmd.none )

        StopTrackWindow ->
            ( { model | isDragging = None }, Cmd.none )

        MouseMove x y ->
            let
                mp =
                    vec2 x y

                delta =
                    sub mp model.mousePosition
            in
            ( { model
                | mousePosition = mp
                , windows = updateWindows_ model mp delta
              }
            , Cmd.none
            )



-- Handle moving and resizing


updateWindows : Model -> a -> Vec2 -> Model
updateWindows m mp delta =
    { m
        | windows =
            updateWindows_ m mp delta
    }


updateWindows_ : Model -> a -> Vec2 -> Array Window
updateWindows_ model _ delta =
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


resizer : (Msg -> msg) -> Corner -> List (Attribute msg) -> String -> Int -> Element msg
resizer toMsg corner attrs c ix =
    el
        ([ onMouseDown (toMsg <| ResizeWindow ix corner)
         , cursor c
         ]
            ++ attrs
        )
        Element.none


cursor : String -> Attribute msg
cursor c =
    htmlAttribute (Html.Attributes.style "cursor" c)



-- View


viewElement : (Msg -> msg) -> Int -> ( Window, Element msg ) -> Element.Attribute msg
viewElement toMsg ix ( position, content ) =
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
            , Element.onLeft
                (resizer toMsg
                    Left
                    [ height fill
                    , width (px rs)
                    , moveRight rs
                    ]
                    "ew-resize"
                    ix
                )
            , Element.onRight
                (resizer toMsg
                    Right
                    [ height fill
                    , width (px rs)
                    , moveLeft rs
                    ]
                    "ew-resize"
                    ix
                )
            , Element.above
                (row [ width fill, moveDown (bw + overhang) ]
                    [ resizer toMsg TopLeft [ height (px rs), width (px rs) ] "nw-resize" ix
                    , resizer toMsg Top [ height (px rs), width fill ] "ns-resize" ix
                    , resizer toMsg TopRight [ height (px rs), width (px rs) ] "ne-resize" ix
                    ]
                )
            , Element.below
                (row [ width fill, moveUp (bw + overhang) ]
                    [ resizer toMsg BottomLeft [ height (px rs), width (px rs) ] "sw-resize" ix
                    , resizer toMsg Bottom [ height (px rs), width fill ] "ns-resize" ix
                    , resizer toMsg BottomRight [ height (px rs), width (px rs) ] "se-resize" ix
                    ]
                )
            ]
         <|
            column
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
                    , onMouseDown (toMsg <| TrackWindow ix)
                    ]
                    []
                , content
                ]
        )


view : (Msg -> msg) -> { a | windows : Array Window } -> List ( c, Element msg ) -> Element msg
view toMsg model windowElements =
    el
        ([ width fill
         , height fill
         , clip
         , onMouseUp (toMsg StopTrackWindow)
         , htmlAttribute
            (Html.Events.on "mousemove"
                (D.map2 MouseMove
                    (D.field "clientX" D.float)
                    (D.field "clientY" D.float)
                    |> D.map toMsg
                )
            )
         ]
            ++ renderWindows toMsg model windowElements
        )
        Element.none


renderWindows : (Msg -> msg) -> { a | windows : Array Window } -> List ( c, Element msg ) -> List (Attribute msg)
renderWindows toMsg model windowElements =
    let
        zipped =
            List.map2
                Tuple.pair
                (Array.toList model.windows)
                (List.map Tuple.second windowElements)
    in
    List.indexedMap (viewElement toMsg) zipped
