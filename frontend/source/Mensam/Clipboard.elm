port module Mensam.Clipboard exposing (copyText)

import Json.Encode as Encode


port copyTextToClipboard : Encode.Value -> Cmd msg


copyText : String -> Cmd msg
copyText =
    Encode.string >> copyTextToClipboard
