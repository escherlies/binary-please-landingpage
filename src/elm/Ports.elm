module Ports exposing (..)

import BrowserWindow exposing (BrowserWindow)
import UI.Theme exposing (Appereance)


type PortMessage
    = UpdateBrowserWindow BrowserWindow
    | UpdatePrefersColorScheme Appereance
