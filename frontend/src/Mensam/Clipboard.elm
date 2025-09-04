module Mensam.Clipboard exposing (copyText)

import Json.Encode as Encode


copyTextToClipboard : Encode.Value -> Cmd msg
copyTextToClipboard _ = Cmd.none


copyText : String -> Cmd msg
copyText =
    Encode.string >> copyTextToClipboard
