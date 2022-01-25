{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module LBKiirotori.Schedule.Data (
    ScheduleRunnerConfig (..)
  , ScheduleRunner (..)
  , withScheduleRunner
) where

import           Control.Exception.Safe        (bracket)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Trans.Reader    (ReaderT, asks)
import qualified Database.Redis                as R

import           LBKiirotori.AccessToken.Class (AccessTokenMonad (..))
import           LBKiirotori.Config            (LBKiirotoriConfig (..),
                                                LBKiirotoriLineConfig (..))
import qualified LBKiirotori.Database.Redis    as Redis

data ScheduleRunnerConfig = ScheduleRunnerConfig {
    srcRedisConn :: R.Connection
  , srcCfg       :: LBKiirotoriConfig
  }

type ScheduleRunner = ReaderT ScheduleRunnerConfig IO

instance AccessTokenMonad ScheduleRunner where
    lineJWKSet = asks $ cfgJWKSet . cfgLine . srcCfg
    lineKId = asks $ cfgKID . cfgLine . srcCfg
    lineChanId = asks $ cfgChannelID . cfgLine . srcCfg
    lineChanSecret = asks $ cfgChannelSecret . cfgLine . srcCfg
    redis rExpr = asks srcRedisConn
        >>= liftIO . flip R.runRedis rExpr

withScheduleRunner :: LBKiirotoriConfig
    -> (ScheduleRunnerConfig -> IO a)
    -> IO a
withScheduleRunner cfg = bracket
    (ScheduleRunnerConfig <$> Redis.newConn (cfgRedis cfg) <*> pure cfg)
    (R.disconnect . srcRedisConn)
