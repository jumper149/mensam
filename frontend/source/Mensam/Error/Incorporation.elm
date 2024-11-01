module Mensam.Error.Incorporation exposing (..)

import Dict
import Mensam.Error
import Time


type IncorporatedErrors
    = MkIncorporatedErrors (Dict.Dict Int (List Mensam.Error.Error))


init : IncorporatedErrors
init =
    MkIncorporatedErrors Dict.empty


incorporate : Time.Posix -> Mensam.Error.Error -> IncorporatedErrors -> IncorporatedErrors
incorporate time error (MkIncorporatedErrors incorporated) =
    let
        f maybeErrors =
            case maybeErrors of
                Nothing ->
                    Just [ error ]

                Just errors ->
                    Just <| error :: errors
    in
    MkIncorporatedErrors <| Dict.update (Time.posixToMillis time) f incorporated


select :
    Maybe { now : Time.Posix, millisIntoThePast : Int }
    -> IncorporatedErrors
    ->
        List
            { error : Mensam.Error.Error
            , time : Time.Posix
            }
select maybeTimeRange (MkIncorporatedErrors incorporated) =
    let
        isInTimeRange ( millis, _ ) =
            case maybeTimeRange of
                Nothing ->
                    True

                Just { now, millisIntoThePast } ->
                    Time.posixToMillis now - millis <= millisIntoThePast

        takeWhile : (a -> Bool) -> List a -> List a
        takeWhile p ls =
            case ls of
                [] ->
                    []

                x :: xs ->
                    if p x then
                        x :: takeWhile p xs

                    else
                        []

        flattenTimeMultiples :
            List ( Int, List Mensam.Error.Error )
            -> List ( Int, Mensam.Error.Error )
        flattenTimeMultiples ls =
            case ls of
                [] ->
                    []

                ( n, errs ) :: rest ->
                    List.map (\err -> ( n, err )) (List.reverse errs) ++ flattenTimeMultiples rest

        toOutputFormat :
            ( Int, Mensam.Error.Error )
            ->
                { error : Mensam.Error.Error
                , time : Time.Posix
                }
        toOutputFormat ( n, err ) =
            { error = err
            , time = Time.millisToPosix n
            }
    in
    List.map toOutputFormat <|
        flattenTimeMultiples <|
            takeWhile isInTimeRange <|
                List.reverse <|
                    Dict.toList incorporated
