module LBKiirotori.Webhook.EventObject.LineBotHandler (
    LineBotHandlerConfig (..)
  , LineBotHandler
) where

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Reader (ReaderT)
import qualified Data.ByteString      as B
import           Servant.Server       (Handler)

import           Database.Redis       (Connection)

data LineBotHandlerConfig = LineBotHandlerConfig {
    lbhChannelSecret :: B.ByteString
  , lbhRedisConn     :: Connection
  }

type LineBotHandler = ReaderT LineBotHandlerConfig (LoggingT Handler)
