module Mensam.Configuration.BaseUrl where

import Mensam.Aeson

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Data.Word
import Deriving.Aeson qualified as A
import GHC.Generics

displayBaseUrl :: BaseUrl -> T.Text
displayBaseUrl baseUrl@BaseUrl {baseUrlScheme, baseUrlAuthority} =
  let absolutePath = displayBaseUrlPath baseUrl <> "/"
   in baseUrlScheme <> ":" <> maybe "" displayBaseUrlAuthority baseUrlAuthority <> absolutePath

displayBaseUrlPath :: BaseUrl -> T.Text
displayBaseUrlPath BaseUrl {baseUrlPath} = T.concat $ map ("/" <>) baseUrlPath

type BaseUrl :: Type
data BaseUrl = BaseUrl
  { baseUrlScheme :: T.Text
  , baseUrlAuthority :: Maybe BaseUrlAuthority
  , baseUrlPath :: [T.Text]
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "" "baseUrl") BaseUrl

displayBaseUrlAuthority :: BaseUrlAuthority -> T.Text
displayBaseUrlAuthority BaseUrlAuthority {baseUrlAuthorityHost, baseUrlAuthorityPort} =
  "//" <> baseUrlAuthorityHost <> maybe "" ((":" <>) . T.pack . show) baseUrlAuthorityPort

type BaseUrlAuthority :: Type
data BaseUrlAuthority = BaseUrlAuthority
  { baseUrlAuthorityHost :: T.Text
  , baseUrlAuthorityPort :: Maybe Word16
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "BaseUrl" "baseUrlAuthority") BaseUrlAuthority
