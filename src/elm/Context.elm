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



-- {-| App context composed of all contexts
-- -}


type alias Context a =
    WithBrowserWindow (UiContext (LangContext (VersionContext a)))
