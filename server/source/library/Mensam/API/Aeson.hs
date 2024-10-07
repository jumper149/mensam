module Mensam.API.Aeson where

import Data.Aeson qualified as A
import Data.Kind
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

type ErrorParseBodyJpeg :: Type
newtype ErrorParseBodyJpeg = MkErrorParseBodyJpeg
  { errorParseBodyJpegError :: String
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via A.CustomJSON (JSONSettings "Mk" "errorParseBodyJpeg") ErrorParseBodyJpeg
