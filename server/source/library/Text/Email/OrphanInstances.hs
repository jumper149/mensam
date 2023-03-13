{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Email.OrphanInstances () where

import Control.Lens
import Data.Aeson qualified as A
import Data.OpenApi
import Data.Proxy
import Data.Text qualified as T
import Text.Email.Parser
import Text.Email.Text

instance A.FromJSON EmailAddress where
  parseJSON json = do
    text <- A.parseJSON @T.Text json
    case fromText text of
      Left err -> fail err
      Right emailAddress -> pure emailAddress

instance A.ToJSON EmailAddress where
  toJSON = A.String . toText

instance ToSchema EmailAddress where
  declareNamedSchema Proxy =
    pure $
      NamedSchema (Just "EmailAddress") $
        mempty
          & type_ ?~ OpenApiString
          & format ?~ "email"
