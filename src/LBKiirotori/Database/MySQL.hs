{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module LBKiirotori.Database.MySQL (
    newConn
) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Database.MySQL.Base        (ConnectInfo, MySQLConn,
                                             MySQLValue (..), connect)

import           LBKiirotori.Internal.Utils (tshow)

newConn :: MonadIO m => ConnectInfo -> m MySQLConn
newConn = liftIO . connect
