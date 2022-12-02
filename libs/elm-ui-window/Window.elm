module Window exposing (..)

import Array exposing (Array)
import Element exposing (Attribute, Element, column, el, fill, height, htmlAttribute, moveDown, moveLeft, moveRight, moveUp, px, row, width)
import Element.Border
import Element.Events exposing (onMouseDown)
import Html.Attributes
import Math.Vector2 exposing (Vec2, add, getX, getY, scale)


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
    }


init : List ( a, b ) -> { windows : Array a, isDragging : Dragging }
init windowElements =
    { windows = Array.fromList <| List.map Tuple.first windowElements
    , isDragging = None
    }


type Msg
    = TrackWindow Int
    | ResizeWindow Int Corner
    | StopTrackWindow


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ResizeWindow ix dir ->
            ( { model | isDragging = Reszie ix dir }, Cmd.none )

        TrackWindow ix ->
            ( { model | isDragging = Move ix }, Cmd.none )

        StopTrackWindow ->
            ( { model | isDragging = None }, Cmd.none )


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
                    , onMouseDown (TrackWindow ix)
                    ]
                    []
                , content
                ]
        )


cursor : String -> Attribute msg
cursor c =
    htmlAttribute (Html.Attributes.style "cursor" c)
