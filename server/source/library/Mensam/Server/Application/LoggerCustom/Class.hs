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

withoutFontEffects :: LogStr -> LogStrWithFontEffects
withoutFontEffects logStr = MkLogStrWithFontEffectsUnsafe [Left logStr]

withFontEffects :: FontEffects -> LogStr -> LogStrWithFontEffects
withFontEffects fontEffects logStr = MkLogStrWithFontEffectsUnsafe [Right (logStr, fontEffects)]

type LogStrWithFontEffects :: Type
newtype LogStrWithFontEffects = MkLogStrWithFontEffectsUnsafe {unLogStrWithFontEffects :: [Either LogStr (LogStr, FontEffects)]}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance IsString LogStrWithFontEffects where
  fromString = MkLogStrWithFontEffectsUnsafe . (: []) . Left . toLogStr

type FontEffects :: Type
newtype FontEffects = MkFontEffects {unFontEffects :: [Int]}
  deriving stock (Eq, Generic, Ord, Read, Show)

renderLogStrWithFontEffectsUnsafe :: Bool -> LogStrWithFontEffects -> LogStr
renderLogStrWithFontEffectsUnsafe colorCapability = \case
  MkLogStrWithFontEffectsUnsafe [] -> ""
  MkLogStrWithFontEffectsUnsafe (part : parts) ->
    let
      renderedPart =
        case part of
          Left logStr -> logStr
          Right (logStr, fontEffects) ->
            if colorCapability
              then wrapLogStrWithFontEffects fontEffects logStr
              else logStr
      renderedParts = renderLogStrWithFontEffectsUnsafe colorCapability $ MkLogStrWithFontEffectsUnsafe parts
     in
      renderedPart <> renderedParts
 where
  wrapLogStrWithFontEffects :: FontEffects -> LogStr -> LogStr
  wrapLogStrWithFontEffects fontEffects str =
    fold
      [ "\ESC[" <> fontEffectsRendered <> "m"
      , str
      , "\ESC[0m"
      ]
   where
    fontEffectsRendered = toLogStr $ fold $ L.intersperse ";" $ show <$> 0 : unFontEffects fontEffects
