module LBKiirotori.Webhook.EventObject.LineBotHandler.Core (
    askLineKId
  , askLineChanId
  , askLineChanSecret
  , askLineChanName
  , askLineUserId
  , askLineJWKSet
  , askMySQLConn
  , askRedisConn
  , runSQL
  , runSQL_
  , runRedis
) where

import           Control.Monad.IO.Class                              (MonadIO (..))
import           Control.Monad.Reader                                (asks)
import qualified Data.ByteString                                     as B
import qualified Data.Text                                           as T
import           Data.Vector                                         (Vector)
import           Database.MySQL.Base                                 as M
import qualified Database.Redis                                      as R
import           System.IO.Streams                                   (InputStream)

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

askMySQLConn :: LineBotHandler M.MySQLConn
askMySQLConn = asks lbhMySQLConn

askRedisConn :: LineBotHandler R.Connection
askRedisConn = asks lbhRedisConn

runSQL :: QueryParam p
    => M.Query
    -> [p]
    -> LineBotHandler (Vector M.ColumnDef, InputStream (Vector M.MySQLValue))
runSQL q p = do
    conn <- askMySQLConn
    liftIO $ queryVector conn q p

runSQL_ :: M.Query
    -> LineBotHandler (Vector M.ColumnDef, InputStream (Vector M.MySQLValue))
runSQL_ q = do
    conn <- askMySQLConn
    liftIO $ queryVector_ conn q

runRedis :: R.Redis a -> LineBotHandler a
runRedis rexpr = askRedisConn >>= liftIO . flip R.runRedis rexpr
