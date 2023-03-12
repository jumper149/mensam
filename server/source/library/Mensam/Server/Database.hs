module Mensam.Server.Database where

import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Schema

import Control.Monad
import Database.Selda qualified as Selda

initDatabase :: MonadSeldaPool m => m ()
initDatabase = void $ runSeldaTransactionT $ do
  Selda.createTable tableUser
  Selda.createTable tableSpace
  Selda.createTable tableSpaceUser
  Selda.createTable tableDesk
  Selda.createTable tableReservation
