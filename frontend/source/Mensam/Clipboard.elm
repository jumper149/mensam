port module Mensam.Clipboard exposing (copyText)


port copyTextToClipboard : String -> Cmd msg


copyText : String -> Cmd msg
copyText =
    copyTextToClipboard
