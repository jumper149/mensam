{-# LANGUAGE ApplicativeDo #-}

module Mensam.Server.Options where

import Mensam.Server.Environment

import Data.Foldable
import Data.Kind
import Data.String
import Options.Applicative
import Options.Applicative.Extra
import Options.Applicative.Help.Pretty

type Options :: Type
newtype Options = MkOptions
  { optionUnit :: ()
  }

defaultOptions :: Options
defaultOptions =
  MkOptions
    { optionUnit = ()
    }

parserInfoOptions :: ParserInfo Options
parserInfoOptions =
  info parserOptions $
    fold
      [ header "Mensam Server"
      , progDesc "host a webserver"
      , footerDoc $
          Just $
            vcat
              [ "Environment variables:"
              , indent 2 $ vcat $ fromString <$> envVarHelp
              ]
      ]

parserOptions :: Parser Options
parserOptions = do
  addHelper <-
    helperWith $
      fold
        [ short 'h'
        , long "help"
        , help "display this help message"
        ]
  pure $ addHelper $ MkOptions {optionUnit = ()}
