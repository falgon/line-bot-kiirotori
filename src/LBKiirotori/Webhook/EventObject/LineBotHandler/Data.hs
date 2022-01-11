module LBKiirotori.Webhook.EventObject.LineBotHandler.Data (
    LineBotHandlerConfig (..)
  , LineBotHandler
) where

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Reader (ReaderT)
import           Database.MySQL.Base  (MySQLConn)
import           Database.Redis       (Connection)
import           Servant.Server       (Handler)

import           LBKiirotori.Config   (LBKiirotoriConfig (..))

data LineBotHandlerConfig = LineBotHandlerConfig {
    lbhMySQLConn :: MySQLConn
  , lbhRedisConn :: Connection
  , lbhCfg       :: LBKiirotoriConfig
  }

type LineBotHandler = ReaderT LineBotHandlerConfig (LoggingT Handler)
