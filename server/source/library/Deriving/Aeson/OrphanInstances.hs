{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Deriving.Aeson.OrphanInstances where

import Data.Aeson qualified as A
import Data.Kind
import Data.OpenApi
import Data.OpenApi.Internal.ParamSchema
import Data.OpenApi.Internal.Schema
import Data.Proxy
import Data.Typeable
import Deriving.Aeson qualified as A
import GHC.Generics
import GHC.TypeLits

instance (ToSchemaOptions t, Generic a, GToSchema (Rep a), Typeable a, Typeable (A.CustomJSON t a)) => ToSchema (A.CustomJSON t a) where
  declareNamedSchema Proxy = genericDeclareNamedSchema (schemaOptions $ Proxy @t) $ Proxy @a
instance (ToSchemaOptions t, Generic a, GToParamSchema (Rep a)) => ToParamSchema (A.CustomJSON t a) where
  toParamSchema Proxy = genericToParamSchema (schemaOptions $ Proxy @t) $ Proxy @a

type ToSchemaOptions :: [Type] -> Constraint
class ToSchemaOptions xs where
  schemaOptions :: Proxy xs -> SchemaOptions

instance ToSchemaOptions '[] where
  schemaOptions Proxy = defaultSchemaOptions

instance ToSchemaOptions xs => ToSchemaOptions (A.UnwrapUnaryRecords ': xs) where
  schemaOptions Proxy = (schemaOptions $ Proxy @xs) {unwrapUnaryRecords = True}

-- instance ToSchemaOptions xs => ToSchemaOptions (A.OmitNothingFields ': xs) where
--  schemaOptions Proxy = (schemaOptions $ Proxy @xs) { omitNothingFields = True }

instance ToSchemaOptions xs => ToSchemaOptions (A.RejectUnknownFields ': xs) where
  schemaOptions Proxy = schemaOptions $ Proxy @xs

instance (A.StringModifier f, ToSchemaOptions xs) => ToSchemaOptions (A.FieldLabelModifier f ': xs) where
  schemaOptions Proxy =
    let next = schemaOptions $ Proxy @xs
     in next {fieldLabelModifier = fieldLabelModifier next . A.getStringModifier @f}

instance (A.StringModifier f, ToSchemaOptions xs) => ToSchemaOptions (A.ConstructorTagModifier f ': xs) where
  schemaOptions Proxy =
    let next = schemaOptions $ Proxy @xs
     in next {constructorTagModifier = constructorTagModifier next . A.getStringModifier @f}

instance (KnownSymbol t, KnownSymbol c, ToSchemaOptions xs) => ToSchemaOptions (A.SumTaggedObject t c ': xs) where
  schemaOptions Proxy =
    (schemaOptions $ Proxy @xs)
      { sumEncoding =
          A.TaggedObject
            { A.tagFieldName = symbolVal $ Proxy @t
            , A.contentsFieldName = symbolVal $ Proxy @c
            }
      }
