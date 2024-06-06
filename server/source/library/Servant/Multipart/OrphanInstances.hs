{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Multipart.OrphanInstances where

import Control.Lens
import Data.HashMap.Strict.InsOrd qualified as HM
import Data.Kind
import Data.OpenApi
import Data.Proxy
import Data.Text qualified as T
import Network.HTTP.Media
import Servant.API
import Servant.Multipart.API
import Servant.OpenApi
import Servant.OpenApi.Internal

type HasOpenApiMultipartProperties :: Type -> Constraint
class HasOpenApiMultipartProperties a where
  toOpenApiMultipartProperties :: Proxy a -> HM.InsOrdHashMap T.Text (Referenced Schema)

type MultipartFileBinary :: Type
data MultipartFileBinary

instance HasOpenApiMultipartProperties MultipartFileBinary where
  toOpenApiMultipartProperties Proxy =
    HM.fromList
      [("file", Inline (mempty & type_ ?~ OpenApiString & format ?~ "binary"))]

-- Heavily inspired by a comment on a GitHub issue:
-- https://github.com/biocad/servant-openapi3/issues/19#issuecomment-1022386282
instance (HasOpenApi api, HasOpenApiMultipartProperties a) => HasOpenApi (MultipartForm tag a :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api) & addRequestBody reqBody
   where
    myMediaType = "multipart" // "form-data"
    mySchema =
      Inline @Schema $
        mempty
          & type_ ?~ OpenApiObject
          & properties .~ toOpenApiMultipartProperties (Proxy :: Proxy a)
    reqBody = mempty @RequestBody & content .~ HM.fromList [(myMediaType, mempty & schema ?~ mySchema)]
