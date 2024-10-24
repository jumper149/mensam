module Mensam.Url exposing
    ( BaseUrl
    , absolute
    , decoder
    , full
    , mockUnsafe
    , parsePrefix
    )

import Json.Decode as Decode
import Url.Builder
import Url.Parser exposing ((</>))


type BaseUrl
    = MkBaseUrl
        { scheme : String
        , authority :
            Maybe
                { host : String
                , port_ : Maybe Int
                }
        , path : List String
        }


decoder : Decode.Decoder BaseUrl
decoder =
    Decode.map3 (\scheme authority path -> MkBaseUrl { scheme = scheme, authority = authority, path = path })
        (Decode.field "scheme" Decode.string)
        (Decode.field "authority" <|
            Decode.nullable <|
                Decode.map2 (\host port_ -> { host = host, port_ = port_ })
                    (Decode.field "host" Decode.string)
                    (Decode.field "port" <| Decode.nullable Decode.int)
        )
        (Decode.field "path" (Decode.list Decode.string))


full : BaseUrl -> List String -> List Url.Builder.QueryParameter -> Maybe String -> String
full (MkBaseUrl baseUrl) pathPieces queryParameters anchor =
    let
        root =
            Url.Builder.CrossOrigin <|
                String.concat
                    [ baseUrl.scheme
                    , ":"
                    , case baseUrl.authority of
                        Nothing ->
                            ""

                        Just authority ->
                            String.concat
                                [ "//"
                                , authority.host
                                , case authority.port_ of
                                    Nothing ->
                                        ""

                                    Just port_ ->
                                        ":" ++ String.fromInt port_
                                ]
                    ]

        path =
            baseUrl.path ++ pathPieces
    in
    Url.Builder.custom root path queryParameters anchor


absolute : BaseUrl -> List String -> List Url.Builder.QueryParameter -> String
absolute (MkBaseUrl baseUrl) pathPieces queryParameters =
    Url.Builder.absolute (baseUrl.path ++ pathPieces) queryParameters


parsePrefix : BaseUrl -> Url.Parser.Parser a a
parsePrefix (MkBaseUrl baseUrl) =
    let
        go pathPieces =
            case pathPieces of
                [] ->
                    Url.Parser.top

                p :: ps ->
                    Url.Parser.s p </> go ps
    in
    go baseUrl.path


mockUnsafe : BaseUrl
mockUnsafe =
    MkBaseUrl
        { scheme = "https"
        , authority =
            Just
                { host = "mens.am"
                , port_ = Nothing
                }
        , path = []
        }
