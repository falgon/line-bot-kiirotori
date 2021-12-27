{-# LANGUAGE CPP, DerivingStrategies, GeneralizedNewtypeDeriving,
             OverloadedStrings #-}
module LBKiirotori.Config (
    LBKiirotoriConfig (..)
  , LBKiirotoriAppConfig (..)
  , LBKiirotoriLineConfig (..)
  , readConfig
) where

import           Control.Arrow          ((|||))
import           Control.Exception.Safe (MonadThrow (..), throwString)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Logger   (LoggingT)
import           Control.Monad.Reader   (ReaderT, asks)
import qualified Data.ByteString        as B
import           Data.Functor           ((<&>))
import qualified Data.HashMap.Lazy      as HM
import           Data.Int               (Int64)
import           Data.String            (IsString (..))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.IO           as T
import           Database.Redis         (ConnectInfo (..), Connection,
                                         PortID (..), defaultConnectInfo)
import qualified Path                   as P
import           Servant.Server         (Handler)
import           Text.Toml              (parseTomlDoc)
import           Text.Toml.Types        (Node (..), Table)

newtype LBKiirotoriAppConfig = LBKiirotoriAppConfig {
    cfgAppWelcome :: T.Text
  }
  deriving stock Show
#ifndef RELEASE
  deriving newtype (Semigroup, Monoid)
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
  , cfgRedis :: ConnectInfo
  , cfgLine  :: LBKiirotoriLineConfig
  }
  deriving stock Show

#ifndef RELEASE
instance Semigroup LBKiirotoriConfig where
    (LBKiirotoriConfig l1 _ l3) <> (LBKiirotoriConfig r1 _ r3) =
        LBKiirotoriConfig (l1 <> r1) defaultConnectInfo (l3 <> r3)

instance Monoid LBKiirotoriConfig where
    mempty = LBKiirotoriConfig mempty defaultConnectInfo mempty
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
    _ -> throwString $ "expected table [" <> T.unpack key <> "]"

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

readRedisConfig :: MonadThrow m => Table -> m ConnectInfo
readRedisConfig redisTable = do
    hostname <- lookupString "hostname" redisTable
    port <- lookupInteger "port" redisTable
        <&> PortNumber
    password <- lookupString "password" redisTable
        <&> \x -> if x == mempty then Nothing else Just x
    select <- lookupInteger "select" redisTable
    maxConn <- lookupInteger "max_connections" redisTable
    maxIdle <- lookupInteger "max_idle_time" redisTable
        <&> fromInteger
    pure $ defaultConnectInfo {
        connectHost = hostname
      , connectPort = port
      , connectAuth = password
      , connectDatabase = select
      , connectMaxConnections = maxConn
      , connectMaxIdleTime = maxIdle
      }

readConfig :: (MonadIO m, MonadThrow m)
    => P.SomeBase P.File
    -> m LBKiirotoriConfig
readConfig fp = do
    tables <- readToml fp
    appConfig <- lookupTable "app" tables
        >>= lookupString "welcome_message"
        <&> LBKiirotoriAppConfig
    redisConfig <- lookupTable "redis" tables
        >>= readRedisConfig
    lineTable <- lookupTable "line" tables
    lineConfig <- LBKiirotoriLineConfig
        <$> lookupString "jwk_set_key" lineTable
        <*> lookupString "kid" lineTable
        <*> lookupString "channel_id" lineTable
        <*> lookupString "channel_secret" lineTable
        <*> lookupString "channel_name" lineTable
        <*> lookupString "user_id" lineTable
    pure $ LBKiirotoriConfig appConfig redisConfig lineConfig

