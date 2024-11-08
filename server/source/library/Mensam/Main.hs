{-# LANGUAGE ApplicativeDo #-}

module Mensam.Main where

import Mensam.Client qualified
import Mensam.Server qualified

import Data.Foldable
import Data.Kind
import Options.Applicative
import Options.Applicative.Extra
import Options.Applicative.Types

main :: IO ()
main = do
  options <- customExecParser parserPrefs parserInfoOptions
  case optionExecute options of
    MkExecutableServer () -> Mensam.Server.main
    MkExecutableClient () -> Mensam.Client.main

parserPrefs :: ParserPrefs
parserPrefs =
  ParserPrefs
    { prefMultiSuffix = ""
    , prefDisambiguate = False
    , prefShowHelpOnError = True
    , prefShowHelpOnEmpty = True
    , prefBacktrack = NoBacktrack
    , prefColumns = 80
    , prefHelpLongEquals = True
    , prefHelpShowGlobal = True
    , prefTabulateFill = 10
    }

type Options :: Type
data Options = MkOptions
  { optionUnit :: ()
  , optionExecute :: Executable
  }

type Executable :: Type
data Executable
  = MkExecutableServer ()
  | MkExecutableClient ()

parserInfoOptions :: ParserInfo Options
parserInfoOptions =
  info parserOptions $
    fold
      [ fullDesc
      , header "Mensam"
      ]

parserOptions :: Parser Options
parserOptions =
  parserAddHelper <*> do
    execute <-
      subparser $
        fold
          [ command "server" (MkExecutableServer <$> parserServerOptions)
          , command "client" (MkExecutableClient <$> parserClientOptions)
          ]
    pure $
      MkOptions
        { optionUnit = ()
        , optionExecute = execute
        }

parserServerOptions :: ParserInfo ()
parserServerOptions =
  info
    (parserAddHelper <*> pure ())
    ( fold
        [ progDesc "host a webserver"
        ]
    )

parserClientOptions :: ParserInfo ()
parserClientOptions =
  info
    (parserAddHelper <*> pure ())
    ( fold
        [ progDesc "connect to a webserver"
        ]
    )

parserAddHelper :: Parser (a -> a)
parserAddHelper =
  helperWith $
    fold
      [ short 'h'
      , long "help"
      , help "display this help message"
      ]