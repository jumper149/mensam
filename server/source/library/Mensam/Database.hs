module Mensam.Database where

import Mensam.Application.SeldaPool.Class
import Mensam.Database.Schema

import Control.Monad
import Database.Selda qualified as Selda

initDatabase :: MonadSeldaPool m => m ()
initDatabase = void $ runSeldaTransactionT $ do
  Selda.createTable tableUser
  Selda.createTable tableSpace
  Selda.createTable tableSpaceUser
  Selda.createTable tableDesk
  Selda.createTable tableReservation
