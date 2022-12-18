module Ports exposing (..)

import BrowserWindow exposing (BrowserWindow)
import Json.Decode as D exposing (Value)
import UI.Theme exposing (Appereance, decodeColorScheme)


type PortMessage
    = UpdateBrowserWindow BrowserWindow
    | UpdatePrefersColorScheme Appereance


decodePortMessage : Value -> Result String PortMessage
decodePortMessage dv =
    dv
        |> D.decodeValue
            (D.field "tag" D.string
                |> D.andThen
                    (\tag ->
                        case tag of
                            "UpdatePrefersColorScheme" ->
                                D.map UpdatePrefersColorScheme (D.field "value" decodeColorScheme)

                            "UpdateBrowserWindow" ->
                                D.map UpdateBrowserWindow (D.field "value" BrowserWindow.decode)

                            other ->
                                D.fail <| "Unkowm message type" ++ other
                    )
            )
        |> Result.mapError D.errorToString
