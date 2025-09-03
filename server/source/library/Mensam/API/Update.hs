module Mensam.API.Update where

import Data.Aeson qualified as A
import Data.Kind
import GHC.Generics

type Updatable :: Type -> Type
type role Updatable _
data Updatable a
  = Preserve
  | Overwrite a
  deriving stock (Eq, Generic, Ord, Read, Show)

instance A.FromJSON a => A.FromJSON (Updatable a) where
  parseJSON = A.withObject "Updatable" $ \v -> do
    isUpdate :: Bool <- v A..: "update"
    if isUpdate
      then do
        value :: a <- v A..: "value"
        pure $ Overwrite value
      else do
        value :: Maybe A.Value <- v A..:! "value"
        case value of
          Nothing -> pure Preserve
          Just _ -> fail "Expected no value to be present, because the 'update' field is false."

instance A.ToJSON a => A.ToJSON (Updatable a) where
  toJSON = \case
    Preserve ->
      A.object
        [ ("update", A.Bool False)
        ]
    Overwrite value ->
      A.object
        [ ("update", A.Bool True)
        , ("value", A.toJSON value)
        ]
