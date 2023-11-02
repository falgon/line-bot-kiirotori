{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module LBKiirotori.Config (
    LBKiirotoriConfig (..)
  , LBKiirotoriAppConfig (..)
  , LBKiirotoriLineConfig (..)
  , readConfigWithLog
) where

import           Control.Arrow                   ((|||))
import           Control.Exception.Safe          (MonadThrow (..), throwString)
import           Control.Monad                   (liftM4, unless)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Logger            (LoggingT)
import qualified Data.ByteString                 as B
import           Data.Functor                    ((<&>))
import qualified Data.HashMap.Lazy               as HM
import           Data.Int                        (Int64)
import           Data.String                     (IsString (..))
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import qualified Database.MySQL.Base             as MySQL
import qualified Database.Redis                  as Redis
import           Network.Wai.Handler.Warp        (Port)
import qualified Options.Applicative.Help.Pretty as OA
import qualified Path                            as P
import           Servant.Server                  (Handler)
import           System.IO                       (hFlush, stdout)
import           Text.Toml                       (parseTomlDoc)
import           Text.Toml.Types                 (Node (..), Table)

data LBKiirotoriAppConfig = LBKiirotoriAppConfig {
    cfgAppWelcome     :: T.Text
  , cfgAppDuringAuth  :: T.Text
  , cfgAppSuccessAuth :: T.Text
  , cfgAppFailedAuth  :: T.Text
  , cfgAppAlreadyAuth :: T.Text
  , cfgAppUnknown     :: T.Text
  , cfgAppPort        :: Port
  , cfgAppRetryMax    :: Int
  }
  deriving stock Show
#ifndef RELEASE
instance Semigroup LBKiirotoriAppConfig where
    (LBKiirotoriAppConfig l1 l2 l3 l4 l5 l6 _ l8) <> (LBKiirotoriAppConfig r1 r2 r3 r4 r5 r6 _ r8) =
        LBKiirotoriAppConfig (l1 <> r1) (l2 <> r2) (l3 <> r3) (l4 <> r4) (l5 <> r5) (l6 <> r6) 80 (l8 + r8)

instance Monoid LBKiirotoriAppConfig where
    mempty = LBKiirotoriAppConfig mempty mempty mempty mempty mempty mempty 80 0
#endif

data LBKiirotoriLineConfig = LBKiirotoriLineConfig {
    cfgJWKSet        :: B.ByteString
  , cfgKID           :: B.ByteString
  , cfgChannelID     :: B.ByteString
  , cfgChannelSecret :: B.ByteString
  , cfgChannelName   :: T.Text
  , cfgUserID        :: B.ByteString
  }
  deriving stock Show

#ifndef RELEASE
instance Semigroup LBKiirotoriLineConfig where
    (LBKiirotoriLineConfig l1 l2 l3 l4 l5 l6) <> (LBKiirotoriLineConfig r1 r2 r3 r4 r5 r6) =
        LBKiirotoriLineConfig (l1 <> r1) (l2 <> r2) (l3 <> r3) (l4 <> r4) (l5 <> r5) (l6 <> r6)

instance Monoid LBKiirotoriLineConfig where
    mempty = LBKiirotoriLineConfig mempty mempty mempty mempty mempty mempty
#endif

data LBKiirotoriConfig = LBKiirotoriConfig {
    cfgApp   :: LBKiirotoriAppConfig
  , cfgMySQL :: MySQL.ConnectInfo
  , cfgRedis :: Redis.ConnectInfo
  , cfgLine  :: LBKiirotoriLineConfig
  }
  deriving stock Show

#ifndef RELEASE
instance Semigroup LBKiirotoriConfig where
    (LBKiirotoriConfig l1 _ _ l3) <> (LBKiirotoriConfig r1 _ _ r3) =
        LBKiirotoriConfig (l1 <> r1) MySQL.defaultConnectInfo Redis.defaultConnectInfo (l3 <> r3)

instance Monoid LBKiirotoriConfig where
    mempty = LBKiirotoriConfig mempty MySQL.defaultConnectInfo Redis.defaultConnectInfo mempty
#endif

readToml :: (MonadIO m, MonadThrow m)
    => P.SomeBase P.File
    -> m Table
readToml fp = liftIO (T.readFile fpath)
    >>= ((throwString . show) ||| pure) . parseTomlDoc fpath
    where
        fpath = P.fromSomeFile fp

lookupTable :: MonadThrow m
    => T.Text
    -> Table
    -> m Table
lookupTable key tb = case HM.lookup key tb of
    Just (VTable tb) -> pure tb
    _                -> throwString $ "expected table [" <> T.unpack key <> "]"

lookupString :: (IsString s, MonadThrow m)
    => T.Text
    -> Table
    -> m s
lookupString key tb = case HM.lookup key tb of
    Just (VString s) -> pure $ fromString $ T.unpack s
    _                -> throwString $ "expected string " <> T.unpack key

lookupInteger :: (Integral i, MonadThrow m)
    => T.Text
    -> Table
    -> m i
lookupInteger key tb = case HM.lookup key tb of
    Just (VInteger v) -> pure $ fromIntegral v
    _                 -> throwString $ "expected integer " <> T.unpack key

readAppConfig :: MonadThrow m => Table -> m LBKiirotoriAppConfig
readAppConfig tb = LBKiirotoriAppConfig
    <$> lookupString "welcome_message" tb
    <*> lookupString "during_auth" tb
    <*> lookupString "success_auth" tb
    <*> lookupString "failed_auth" tb
    <*> lookupString "already_auth" tb
    <*> lookupString "unknown_cmd_message" tb
    <*> lookupInteger "port" tb
    <*> lookupInteger "retry_max" tb

readRedisConfig :: MonadThrow m => Table -> m Redis.ConnectInfo
readRedisConfig redisTable = do
    hostname <- lookupString "hostname" redisTable
    port <- lookupInteger "port" redisTable
        <&> Redis.PortNumber
    password <- lookupString "password" redisTable
        <&> \x -> if x == mempty then Nothing else Just x
    select <- lookupInteger "select" redisTable
    maxConn <- lookupInteger "max_connections" redisTable
    maxIdle <- lookupInteger "max_idle_time" redisTable
        <&> fromInteger
    pure $ Redis.defaultConnectInfo {
        Redis.connectHost = hostname
      , Redis.connectPort = port
      , Redis.connectAuth = password
      , Redis.connectDatabase = select
      , Redis.connectMaxConnections = maxConn
      , Redis.connectMaxIdleTime = maxIdle
      }

readMySQLConfig :: MonadThrow m => Table -> m MySQL.ConnectInfo
readMySQLConfig rdbTable = do
    hostname <- lookupString "hostname" rdbTable
    port <- lookupInteger "port" rdbTable
    password <- lookupString "password" rdbTable
    user <- lookupString "username" rdbTable
    db <- lookupString "database" rdbTable
    charSet <- lookupInteger "charset" rdbTable
    pure $ MySQL.defaultConnectInfo {
        MySQL.ciHost = hostname
      , MySQL.ciPort = port
      , MySQL.ciDatabase = db
      , MySQL.ciUser = user
      , MySQL.ciPassword = password
      , MySQL.ciCharset = charSet
      }

readLineConfig :: MonadThrow m => Table -> m LBKiirotoriLineConfig
readLineConfig lineTable = LBKiirotoriLineConfig
    <$> lookupString "jwk_set_key" lineTable
    <*> lookupString "kid" lineTable
    <*> lookupString "channel_id" lineTable
    <*> lookupString "channel_secret" lineTable
    <*> lookupString "channel_name" lineTable
    <*> lookupString "user_id" lineTable

readConfig :: (MonadIO m, MonadThrow m)
    => P.SomeBase P.File
    -> m LBKiirotoriConfig
readConfig fp = do
    tables <- readToml fp
    liftM4 LBKiirotoriConfig
        (lookupTable "app" tables >>= readAppConfig)
        (lookupTable "mysql" tables >>= readMySQLConfig)
        (lookupTable "redis" tables >>= readRedisConfig)
        (lookupTable "line" tables >>= readLineConfig)

readConfigWithLog :: (MonadIO m, MonadThrow m)
    => Bool
    -> P.SomeBase P.File
    -> m LBKiirotoriConfig
readConfigWithLog isQuiet fp = unless isQuiet (liftIO $ putStr "reading config file ... " *> hFlush stdout)
    *> readConfig fp
    <* unless isQuiet (liftIO $ OA.putDoc (OA.dullgreen $ OA.text "done" <> OA.hardline))
