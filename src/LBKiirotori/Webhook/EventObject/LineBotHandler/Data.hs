module LBKiirotori.Webhook.EventObject.LineBotHandler.Data (
    LineBotHandlerConfig (..)
  , LineBotHandler
) where

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Reader (ReaderT)
import           Database.Redis       (Connection)
import           Servant.Server       (Handler)

import           LBKiirotori.Config   (LBKiirotoriConfig (..))

data LineBotHandlerConfig = LineBotHandlerConfig {
    lbhRedisConn :: Connection
  , lbhCfg       :: LBKiirotoriConfig
  }

type LineBotHandler = ReaderT LineBotHandlerConfig (LoggingT Handler)
