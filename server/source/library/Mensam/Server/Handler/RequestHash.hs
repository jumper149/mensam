{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Handler.RequestHash where

import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.ByteString.Char8 qualified as B
import Data.Hashable qualified
import Data.Kind
import Network.Wai
import Servant
import Servant.Server.Internal.Delayed

type Hash :: Type
newtype Hash = MkHash {getHash :: Word}

instance Show Hash where
  show hash = paddingString ++ hashString
   where
    hashString = show $ getHash hash
    paddingString = replicate paddingLength '0'
    paddingLength = maxHashLength - length hashString
     where
      maxHashLength = length $ show $ getHash $ MkHash maxBound

requestHash :: Request -> Hash
requestHash = MkHash . fromIntegral . Data.Hashable.hash . show

type RequestHashT :: (Type -> Type) -> Type -> Type
newtype RequestHashT m a = RequestHashT {unRequestHashT :: ReaderT Hash m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance MonadLogger m => MonadLogger (RequestHashT m) where
  monadLoggerLog loc logSource logLevel logStr = do
    reqHash <- RequestHashT ask
    let reqInfo = "#[" <> show reqHash <> "]"
    lift . monadLoggerLog loc logSource logLevel . toLogStr $
      B.pack reqInfo <> " " <> fromLogStr (toLogStr logStr)

deriving via
  RequestHashT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadLogger (t2 m) => MonadLogger (ComposeT RequestHashT t2 m)

runRequestHashT :: Hash -> RequestHashT m a -> m a
runRequestHashT reqHash = flip runReaderT reqHash . unRequestHashT

type RequestHash :: Type
data RequestHash

instance HasServer api context => HasServer (RequestHash :> api) context where
  type ServerT (RequestHash :> api) m = Hash -> ServerT api m
  hoistServerWithContext Proxy pc nt s = hoistServerWithContext (Proxy @api) pc nt . s
  route Proxy context subserver = route (Proxy @api) context $ passToServer subserver requestHash
