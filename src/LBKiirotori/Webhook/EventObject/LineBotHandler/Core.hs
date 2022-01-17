{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
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
  , executeSQL
  , runRedis
) where

import           Control.Monad.IO.Class                              (MonadIO (..))
import           Control.Monad.Reader                                (asks)
import qualified Data.ByteString                                     as B
import qualified Data.Text                                           as T
import           Data.Vector                                         (Vector)
import qualified Database.MySQL.Base                                 as M
import qualified Database.Redis                                      as R
import           System.IO.Streams                                   (InputStream)

import qualified LBKiirotori.AccessToken.Class                       as AC
import           LBKiirotori.Config                                  (LBKiirotoriConfig (..),
                                                                      LBKiirotoriLineConfig (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler.Data (LineBotHandler,
                                                                      LineBotHandlerConfig (..))

instance AC.AccessTokenMonad LineBotHandler where
    lineJWKSet = askLineJWKSet
    lineKId = askLineKId
    lineChanId = askLineChanId
    lineChanSecret = askLineChanSecret
    redis = runRedis

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

runSQL :: M.QueryParam p
    => M.Query
    -> [p]
    -> LineBotHandler (Vector M.ColumnDef, InputStream (Vector M.MySQLValue))
runSQL q p = askMySQLConn
    >>= liftIO . flip (flip M.queryVector q) p

runSQL_ :: M.Query
    -> LineBotHandler (Vector M.ColumnDef, InputStream (Vector M.MySQLValue))
runSQL_ q = askMySQLConn
    >>= liftIO . flip M.queryVector_ q

executeSQL :: M.QueryParam p
    => M.Query
    -> [p]
    -> LineBotHandler M.OK
executeSQL q p = askMySQLConn
    >>= liftIO . flip (flip M.execute q) p

runRedis :: R.Redis a -> LineBotHandler a
runRedis rexpr = askRedisConn >>= liftIO . flip R.runRedis rexpr
