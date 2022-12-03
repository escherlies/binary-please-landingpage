module Window exposing (..)

import Array exposing (Array)
import Element exposing (Attribute, Element, clip, el, fill, height, htmlAttribute, moveDown, moveLeft, moveRight, moveUp, px, row, width)
import Element.Events exposing (onMouseDown, onMouseUp)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as D
import Math.Vector2 exposing (Vec2, add, getX, getY, scale, setX, setY, sub, vec2)


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


type Drag
    = None
    | Reszie Int Corner
    | Move Int


type alias Model =
    { windows : Array Window
    , drag : Drag
    , mousePosition : Vec2
    }


init : List ( Window, b ) -> Model
init windowElements =
    { windows = Array.fromList <| List.map Tuple.first windowElements
    , drag = None
    , mousePosition = vec2 0 0
    }


empty : Model
empty =
    { windows = Array.empty
    , drag = None
    , mousePosition = vec2 0 0
    }


type Msg
    = TrackWindow Int
    | ResizeWindow Int Corner
    | StopTrackWindow
    | MouseMove Vec2


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ResizeWindow ix dir ->
            ( { model | drag = Reszie ix dir }, Cmd.none )

        TrackWindow ix ->
            ( { model | drag = Move ix }, Cmd.none )

        StopTrackWindow ->
            ( { model | drag = None }, Cmd.none )

        MouseMove mp ->
            ( { model
                | mousePosition = mp
                , windows = updateWindows model mp
              }
            , Cmd.none
            )



-- Handle moving and resizing


updateWindows : Model -> Vec2 -> Array Window
updateWindows model mp =
    case model.drag of
        None ->
            model.windows

        Move ix ->
            let
                cw =
                    Array.get ix model.windows

                delta =
                    sub mp model.mousePosition
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

                delta =
                    sub mp model.mousePosition
            in
            case cw of
                Just wp ->
                    handleRezise ix wp corner delta model.windows

                Nothing ->
                    -- Should never happen
                    model.windows


handleRezise : Int -> Window -> Corner -> Vec2 -> Array Window -> Array Window
handleRezise ix wp corner delta windows =
    (case corner of
        Bottom ->
            { wp
                | size = add wp.size (setX 0 delta)
            }

        Top ->
            { wp
                | size = add wp.size (setX 0 delta |> scale -1)
                , position = add wp.position (setX 0 delta)
            }

        Right ->
            { wp
                | size = add wp.size (setY 0 delta)
            }

        Left ->
            { wp
                | size = add wp.size (setY 0 delta |> scale -1)
                , position = add wp.position (setY 0 delta)
            }

        BottomRight ->
            { wp
                | size = add wp.size delta
            }

        BottomLeft ->
            { wp
                | size =
                    add wp.size (setY 0 delta |> scale -1)
                        |> (\size -> add size (setX 0 delta))
                , position = add wp.position (setY 0 delta)
            }

        TopRight ->
            { wp
                | size =
                    add wp.size (setX 0 delta |> scale -1)
                        |> (\size -> add size (setY 0 delta))
                , position = add wp.position (setX 0 delta)
            }

        TopLeft ->
            { wp
                | size = add wp.size (delta |> scale -1)
                , position = add wp.position delta
            }
    )
        |> (\w ->
                if getX w.size < 100 then
                    { w
                        | size = setX 100 w.size
                        , position = setX (getX wp.position) w.position
                    }

                else
                    w
           )
        |> (\w ->
                if getY w.size < 100 then
                    { w
                        | size = setY 100 w.size
                        , position = setY (getY wp.position) w.position
                    }

                else
                    w
           )
        |> (\w -> Array.set ix w windows)


resizer : (Msg -> msg) -> Corner -> List (Attribute msg) -> String -> Int -> Element msg
resizer toMsg corner attrs c ix =
    el
        ([ onMouseDown (toMsg <| ResizeWindow ix corner)
         , cursor c
         ]
            ++ attrs
        )
        Element.none



--


cursor : String -> Attribute msg
cursor c =
    htmlAttribute (Html.Attributes.style "cursor" c)


pointerEventsNone : Attribute msg
pointerEventsNone =
    htmlAttribute (Html.Attributes.style "pointer-events" "none")


pointerEventsAuto : Attribute msg
pointerEventsAuto =
    htmlAttribute (Html.Attributes.style "pointer-events" "auto")


userSelect : Bool -> List (Element.Attribute msg)
userSelect val =
    if val then
        []

    else
        [ Element.htmlAttribute (style "user-select" "none")
        , Element.htmlAttribute (style "-ms-user-select" "none")
        , Element.htmlAttribute (style "-moz-user-select" "none")
        , Element.htmlAttribute (style "-webkit-user-select" "none")
        , Element.htmlAttribute (style "-webkit-touch-callout" "none")
        ]



-- View


viewElement :
    (Msg -> msg)
    -> Model
    -> Int
    -> ( Window, Int -> Element msg )
    -> Element.Attribute msg
viewElement toMsg model ix ( position, content ) =
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
            ([ Element.moveRight (getX position.position)
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
                ++ userSelect (model.drag == None)
            )
         <|
            content ix
        )


view : (Msg -> msg) -> Model -> List ( c, Int -> Element msg ) -> Element msg
view toMsg model windowElements =
    el
        ([ width fill
         , height fill
         , clip
         , onMouseUp (toMsg StopTrackWindow)
         , htmlAttribute
            (Html.Events.on "mousemove"
                (D.map2 vec2
                    (D.field "clientX" D.float)
                    (D.field "clientY" D.float)
                    |> D.map (toMsg << MouseMove)
                )
            )
         ]
            ++ renderWindows toMsg model windowElements
        )
        Element.none


renderWindows : (Msg -> msg) -> Model -> List ( c, Int -> Element msg ) -> List (Attribute msg)
renderWindows toMsg model windowElements =
    let
        zipped =
            List.map2
                Tuple.pair
                (Array.toList model.windows)
                (List.map Tuple.second windowElements)
    in
    List.indexedMap (viewElement toMsg model) zipped
