module LBKiirotori.Webhook.EventObject.LineBotHandler.Core (
    askLineKId
  , askLineChanId
  , askLineChanSecret
  , askLineChanName
  , askLineUserId
  , askLineJWKSet
  , askRedisConn
  , runRedis
) where

import           Control.Monad.IO.Class                              (MonadIO (..))
import           Control.Monad.Reader                                (asks)
import qualified Data.ByteString                                     as B
import qualified Data.Text                                           as T
import qualified Database.Redis                                      as R

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

askLineChanName :: LineBotHandler T.Text
askLineChanName = asks $ cfgChannelName . cfgLine . lbhCfg

askLineUserId :: LineBotHandler B.ByteString
askLineUserId = asks $ cfgUserID . cfgLine . lbhCfg

askLineJWKSet :: LineBotHandler B.ByteString
askLineJWKSet = asks $ cfgJWKSet . cfgLine . lbhCfg

askRedisConn :: LineBotHandler R.Connection
askRedisConn = asks lbhRedisConn

runRedis :: R.Redis a -> LineBotHandler a
runRedis rexpr = askRedisConn >>= liftIO . flip R.runRedis rexpr
