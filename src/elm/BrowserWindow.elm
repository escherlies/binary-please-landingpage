module BrowserWindow exposing (..)

import Json.Decode as D


type alias BrowserWindow =
    { width : Float
    , height : Float
    }


type alias WithBrowserWindow ctx =
    { ctx
        | window :
            BrowserWindow
    }


decode : D.Decoder BrowserWindow
decode =
    D.map2 BrowserWindow
        (D.field "innerWidth" D.float)
        (D.field "innerHeight" D.float)
