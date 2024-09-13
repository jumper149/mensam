module Mensam.Server.Configuration.SQLite where

import Mensam.API.Aeson

import Control.DeepSeq
import Data.Aeson qualified as A
import Data.Kind
import Deriving.Aeson qualified as A
import GHC.Generics

type SQLiteConfig :: Type
data SQLiteConfig = MkSQLiteConfig
  { sqliteFilepath :: FilePath
  , sqliteConnectionPoolTimeoutSeconds :: Double
  -- ^ Number of seconds, that an unused resource is kept in the pool.
  , sqliteConnectionPoolMaxNumberOfConnections :: Int
  -- ^ Maximum number of resources open at once.
  , sqliteCheckDataIntegrityOnStartup :: Bool
  -- ^ Maximum number of resources open at once.
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (NFData)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "sqlite") SQLiteConfig
