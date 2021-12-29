module LBKiirotori.Database.MySQL (
    newConn
) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Database.MySQL.Base    (ConnectInfo, MySQLConn, connect)

newConn :: MonadIO m => ConnectInfo -> m MySQLConn
newConn = liftIO . connect
