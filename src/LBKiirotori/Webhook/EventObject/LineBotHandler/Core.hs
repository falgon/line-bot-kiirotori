module LBKiirotori.Webhook.EventObject.LineBotHandler.Core (
    askLineKId
  , askLineChanId
  , askLineChanSecret
  , askLineUserId
  , askLineJWKSet
  , askRedisConn
) where

import           Control.Monad.IO.Class                              (MonadIO (..))
import           Control.Monad.Reader                                (asks)
import qualified Data.ByteString                                     as B
import           Database.Redis                                      (Connection)

import           LBKiirotori.Config                                  (LBKiirotoriConfig (..),
                                                                      LBKiirotoriLineConfig (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler.Data (LineBotHandler,
                                                                      LineBotHandlerConfig (..))

askLineKId :: LineBotHandler B.ByteString
askLineKId = asks $ cfgKID . cfgLine . lbhCfg

askLineChanId :: LineBotHandler B.ByteString
askLineChanId = asks $ cfgChannelID . cfgLine . lbhCfg

askLineChanSecret :: LineBotHandler B.ByteString
askLineChanSecret = asks $ cfgChannelSecret . cfgLine . lbhCfg

askLineUserId :: LineBotHandler B.ByteString
askLineUserId = asks $ cfgUserID . cfgLine . lbhCfg

askLineJWKSet :: LineBotHandler B.ByteString
askLineJWKSet = asks $ cfgJWKSet . cfgLine . lbhCfg

askRedisConn :: LineBotHandler Connection
askRedisConn = asks lbhRedisConn

