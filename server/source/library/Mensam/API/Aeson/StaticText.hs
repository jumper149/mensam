{-# LANGUAGE UndecidableInstances #-}

module Mensam.API.Aeson.StaticText where

import Mensam.API.Aeson.StaticText.Internal ()
import Mensam.API.Aeson.StaticText.Internal.Union qualified as Union

import Data.Aeson qualified as A
import Data.Kind
import Data.Proxy
import Data.SOP qualified as SOP
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits

type StaticText :: Symbol -> Type
type role StaticText nominal
data StaticText text = MkStaticText
  deriving stock (Eq, Generic, Ord, Read, Show)

instance KnownSymbol text => A.FromJSON (StaticText text) where
  parseJSON = A.withText ("(StaticText " ++ show str ++ ")") $ \jsonTxt ->
    if jsonTxt == txt
      then pure MkStaticText
      else fail $ "Unexpected static text. Expected :" <> show str
   where
    str = symbolVal (Proxy @text)
    txt = T.pack str

instance KnownSymbol text => A.ToJSON (StaticText text) where
  toJSON MkStaticText = A.String $ T.pack $ symbolVal (Proxy @text)

type StaticTexts :: [Symbol] -> Type
type role StaticTexts nominal
newtype StaticTexts texts = MkStaticTexts {unStaticTexts :: Union.Union (Union.Map StaticText texts)}

deriving stock instance SOP.All (SOP.Compose Eq SOP.I) (Union.Map StaticText texts) => Eq (StaticTexts texts)
deriving stock instance SOP.All (SOP.Compose Show SOP.I) (Union.Map StaticText texts) => Show (StaticTexts texts)
deriving stock instance (SOP.All (SOP.Compose Eq SOP.I) (Union.Map StaticText texts), SOP.All (SOP.Compose Ord SOP.I) (Union.Map StaticText texts)) => Ord (StaticTexts texts)
deriving stock instance Read (StaticTexts '[])
deriving stock instance (Read (StaticText text), Read (SOP.NS SOP.I (Union.Map StaticText texts))) => Read (StaticTexts (text : texts))

deriving newtype instance A.ToJSON (StaticTexts '[])
deriving newtype instance (A.ToJSON (StaticText text), A.ToJSON (SOP.NS SOP.I (Union.Map StaticText texts)), Union.Unique (Union.Map StaticText (text : texts))) => A.ToJSON (StaticTexts (text : texts))

deriving newtype instance A.FromJSON (StaticTexts '[])
deriving newtype instance (A.FromJSON (StaticText text), A.FromJSON (SOP.NS SOP.I (Union.Map StaticText texts)), Union.Unique (Union.Map StaticText (text : texts))) => A.FromJSON (StaticTexts (text : texts))

specificStaticText :: forall texts text. Union.IsMember (StaticText text) (Union.Map StaticText texts) => StaticText text -> StaticTexts texts
specificStaticText staticText = MkStaticTexts $ Union.inject $ SOP.I staticText
