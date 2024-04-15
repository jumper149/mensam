module Http.Extra exposing (..)

import Http


type Status
    = Loading
    | Done


status : Http.Progress -> Status
status progress =
    case progress of
        Http.Sending { size, sent } ->
            if sent == size then
                Done

            else
                Loading

        Http.Receiving { size, received } ->
            case size of
                Nothing ->
                    Done

                Just justSize ->
                    if justSize == received then
                        Done

                    else
                        Loading


tracker : Maybe String
tracker =
    Just "http"
