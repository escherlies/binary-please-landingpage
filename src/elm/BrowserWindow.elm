module BrowserWindow exposing (..)

import Json.Decode as D
import Math.Vector2 exposing (Vec2, vec2)


type alias BrowserWindow =
    Vec2


type alias WithBrowserWindow ctx =
    { ctx
        | window :
            BrowserWindow
    }


decode : D.Decoder BrowserWindow
decode =
    D.map2 vec2
        (D.field "innerWidth" D.float)
        (D.field "innerHeight" D.float)
