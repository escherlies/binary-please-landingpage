module Ports exposing (..)

import BrowserWindow exposing (BrowserWindow)


type PortMessage
    = UpdateBrowserWindow BrowserWindow
