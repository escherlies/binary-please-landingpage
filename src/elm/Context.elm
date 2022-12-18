module Context exposing (..)

import BrowserWindow exposing (WithBrowserWindow)
import UI exposing (UiContext)
import UI.Theme exposing (Appereance(..))


{-| Lang Module
-}
type Lang
    = De
    | En


getTranslation : LangContext ctx -> { a | en : String, de : String } -> String
getTranslation { lang } { en, de } =
    case lang of
        En ->
            en

        De ->
            de


type alias LangContext a =
    { a
        | lang : Lang
    }


{-| Another arbitrary context
-}
type alias VersionContext ctx =
    { ctx
        | version : Int
    }


type alias SettingsContext ctx =
    { ctx
        | debug : Bool
    }


{-| App context composed of all contexts

See <https://gist.github.com/escherlies/b03ae740b6eb2048c108f20fbe2c2b26>

-}
type alias Context a =
    SettingsContext (WithBrowserWindow (UiContext (LangContext (VersionContext a))))
