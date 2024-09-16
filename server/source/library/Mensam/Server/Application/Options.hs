{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Application.Options where

import Mensam.Server.Application.Options.Class
import Mensam.Server.Environment

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.Kind
import Data.String
import Options.Applicative
import Options.Applicative.Extra
import Options.Applicative.Help.Pretty

type OptionsT :: (Type -> Type) -> Type -> Type
newtype OptionsT m a = MkOptionsT {unOptionsT :: ReaderT Options m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance Monad m => MonadOptions (OptionsT m) where
  options = MkOptionsT ask

deriving via
  OptionsT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    Monad (t2 m) => MonadOptions (ComposeT OptionsT t2 m)

runOptionsT :: OptionsT m a -> Options -> m a
runOptionsT = runReaderT . unOptionsT

runAppOptionsT ::
  MonadIO m =>
  OptionsT m a ->
  m a
runAppOptionsT tma = do
  parsedOptions <- liftIO $ execParser parserInfoOptions
  runOptionsT tma parsedOptions

parserInfoOptions :: ParserInfo Options
parserInfoOptions =
  info parserOptions $
    fold
      [ fullDesc
      , header "Mensam"
      , progDescDoc $
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
