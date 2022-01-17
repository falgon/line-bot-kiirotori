module LBKiirotori.Schedule.Job (
    watchSchedule
) where

import           Control.Arrow                   ((&&&))
import           Control.Concurrent              (ThreadId, killThread,
                                                  threadDelay)
import           Control.Exception.Safe          (MonadThrow, bracket)
import           Control.Monad                   (forever, mapM_, unless)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans             (lift)
import           Control.Monad.Trans.Reader      (ReaderT (..))
import           Data.Functor                    ((<&>))
import           Data.IORef                      (newIORef, readIORef,
                                                  writeIORef)
import qualified Data.Text.IO                    as T
import qualified Database.Redis                  as R
import qualified Options.Applicative.Help.Pretty as OA
import qualified Path                            as P
import qualified System.Cron.Schedule            as C
import           System.FSNotify                 (Event (..), eventPath,
                                                  watchDir, withManager)
import           System.IO                       (hFlush, stdout)
import           Text.Printf                     (printf)

import           LBKiirotori.AccessToken         (getAccessToken)
import           LBKiirotori.API.PushMessage     (PushMessage (..), pushMessage)
import           LBKiirotori.Config              (LBKiirotoriConfig (..))
import           LBKiirotori.Data.MessageObject  (MessageBody (..), textMessage)
import qualified LBKiirotori.Database.Redis      as Redis
import           LBKiirotori.Internal.Utils      (mapSomeBase)
import           LBKiirotori.Schedule.Data
import           LBKiirotori.Schedule.Parser

mapAppInstance :: ScheduleRunnerConfig
    -> SchedulableAppRow
    -> IO ()
mapAppInstance cfg (SchedulableAppRow _ tId (SchedulableApp PushTextMessage arg)) =
    flip runReaderT cfg $ getAccessToken
        >>= lift . flip pushMessage messageObject
    where
        messageObject = PushMessage {
            pmTo = tId
          , pmMessages = map (\x -> MBText $ textMessage x Nothing Nothing) arg
          }

readSchedule :: (MonadThrow m, MonadIO m)
    => P.SomeBase P.File
    -> ScheduleRunnerConfig
    -> m (C.Schedule ())
readSchedule fp cfg = liftIO (T.readFile $ P.fromSomeFile fp)
    >>= parseCronSchedule
    <&> mapM_ (cronMapper cfg)
    where
        cronMapper srCfg = uncurry C.addJob . (mapAppInstance cfg &&& sarCronExpr)

runSchedule :: (MonadThrow m, MonadIO m)
    => P.SomeBase P.File
    -> ScheduleRunnerConfig
    -> m [ThreadId]
runSchedule fp cfg = readSchedule fp cfg
    >>= liftIO . C.execSchedule

watchSchedule :: (MonadThrow m, MonadIO m)
    => Bool
    -> P.SomeBase P.File
    -> LBKiirotoriConfig
    -> m ()
watchSchedule qFlag fp cfg = liftIO $ do
    unless qFlag (putStrLn "ready to boot scheduler")
    bracket
        (ScheduleRunnerConfig <$> Redis.newConn (cfgRedis cfg) <*> pure cfg)
        (R.disconnect . srcRedisConn) $ \srCfg -> do
        bracket (runSchedule fp srCfg) (mapM_ killThread) $ \tIds -> do
            tIdsRef <- newIORef tIds
            withManager $ \mgr -> do
                watchDir mgr (P.fromSomeDir $ mapSomeBase P.parent fp) predicate $ \event ->
                    unless qFlag (putUpdateLog event)
                        >> readIORef tIdsRef
                        >>= mapM_ killThread
                        >> runSchedule fp srCfg
                        >>= writeIORef tIdsRef
                forever $ threadDelay 1000000
    where
        predicate (Modified ufp _ _) = Just fp == P.parseSomeFile ufp
        predicate _                  = False

        putUpdateLog = putStrLn
            . printf "update detected of cron file (%s), reloading"
            . eventPath
