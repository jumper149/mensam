module Mensam.API.Aeson where

import Data.Aeson qualified as A
import Data.Kind
import Data.Proxy
import Data.Text qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics
import GHC.TypeLits

type JSONSettings :: Symbol -> Symbol -> [Type]
type JSONSettings constructorPrefix fieldPrefix =
  '[ A.ConstructorTagModifier
      '[ A.StripPrefix constructorPrefix
       , A.CamelToKebab
       ]
   , A.SumTaggedObject "tag" "value"
   , A.FieldLabelModifier
      '[ A.StripPrefix fieldPrefix
       , A.CamelToKebab
       ]
   , A.RejectUnknownFields
   ]

type NameOrIdentifier :: Type -> Type -> Type
data NameOrIdentifier name identifier
  = Name name
  | Identifier identifier
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via A.CustomJSON (JSONSettings "" "") (NameOrIdentifier name identifier)

type ErrorParseBodyJson :: Type
newtype ErrorParseBodyJson = MkErrorParseBodyJson
  { errorParseBodyJsonError :: String
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via A.CustomJSON (JSONSettings "Mk" "errorParseBodyJson") ErrorParseBodyJson

type StaticText :: Symbol -> Type
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
