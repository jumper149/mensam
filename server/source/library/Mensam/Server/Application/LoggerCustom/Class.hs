module Mensam.Server.Application.LoggerCustom.Class where

import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Foldable
import Data.Kind
import Data.List qualified as L
import Data.String
import GHC.Generics

type MonadLoggerCustom :: (Type -> Type) -> Constraint
class MonadLogger m => MonadLoggerCustom m where
  colorfulLogCapability :: m Bool

instance
  ( MonadTrans t
  , MonadLoggerCustom m
  ) =>
  MonadLoggerCustom (Elevator t m)
  where
  colorfulLogCapability = lift colorfulLogCapability

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( MonadTrans t1
    , MonadLoggerCustom (t2 m)
    ) =>
    MonadLoggerCustom (ComposeT t1 t2 m)

wrapLogStrWithFontEffects :: Bool -> FontEffects -> LogStr -> LogStr
wrapLogStrWithFontEffects colorCapability fontEffects str =
  if colorCapability
    then
      fold
        [ "\ESC[0m"
        , "\ESC[" <> fontEffectsRendered <> "m"
        , str
        , "\ESC[0m"
        ]
    else str
 where
  fontEffectsRendered = toLogStr $ fold $ L.intersperse ";" $ show <$> 0 : unFontEffects fontEffects

type LogStrWithFontEffects :: Type
newtype LogStrWithFontEffects = MkLogStrWithFontEffects {unLogStrWithFontEffects :: [Either LogStr (LogStr, FontEffects)]}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance IsString LogStrWithFontEffects where
  fromString = MkLogStrWithFontEffects . (: []) . Left . toLogStr

type FontEffects :: Type
newtype FontEffects = MkFontEffects {unFontEffects :: [Int]}
  deriving stock (Eq, Generic, Ord, Read, Show)

renderLogStrWithFontEffects :: Bool -> LogStrWithFontEffects -> LogStr
renderLogStrWithFontEffects colorCapability = \case
  MkLogStrWithFontEffects [] -> ""
  MkLogStrWithFontEffects (part : parts) ->
    let
      renderedPart =
        case part of
          Left logStr -> logStr
          Right (logStr, fontEffects) ->
            wrapLogStrWithFontEffects colorCapability fontEffects logStr
      renderedParts = renderLogStrWithFontEffects colorCapability $ MkLogStrWithFontEffects parts
     in
      renderedPart <> renderedParts
