module Mensam.Http.Status exposing (..)

import Http


type Status
    = Loading
    | Done



-- TODO: Does not work currently.
-- After adding the ability to cancel requests on screen changes we had to use `tracker` field for that.
-- Now `status` will never be called.


status : Http.Progress -> Status
status progress =
    case progress of
        Http.Sending { size, sent } ->
            if sent == size then
                Loading

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
