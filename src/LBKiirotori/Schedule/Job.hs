module LBKiirotori.Schedule.Job (
    watchSchedule
) where

import           Control.Arrow                   ((&&&))
import           Control.Concurrent              (ThreadId, killThread,
                                                  threadDelay)
import           Control.Concurrent.MVar         (newMVar, putMVar, takeMVar)
import           Control.Exception.Safe          (MonadThrow, bracket)
import           Control.Monad                   (forever, mapM_, unless, (>=>))
import           Control.Monad.Extra             (ifM)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans             (lift)
import           Control.Monad.Trans.Reader      (ReaderT (..), asks)
import           Data.Functor                    ((<&>))
import           Data.IORef                      (IORef, newIORef, readIORef,
                                                  writeIORef)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import qualified Database.Redis                  as R
import qualified Options.Applicative.Help.Pretty as OA
import qualified Path                            as P
import qualified Path.IO                         as P
import qualified System.Cron.Schedule            as C
import qualified System.Cron.Types               as C
import           System.FSNotify                 (Event (..), eventPath,
                                                  watchDir, withManager)
import           System.IO                       (hFlush, stdout)
import           Text.Printf                     (printf)

import           LBKiirotori.AccessToken         (getAccessToken)
import           LBKiirotori.API.PushMessage     (PushMessage (..), pushMessage)
import           LBKiirotori.Config              (LBKiirotoriAppConfig (..),
                                                  LBKiirotoriConfig (..))
import           LBKiirotori.Data.MessageObject  (MessageBody (..), textMessage)
import           LBKiirotori.Internal.Utils      (mapSomeBase, prjSomeBaseM,
                                                  tshow)
import           LBKiirotori.Schedule.Data
import           LBKiirotori.Schedule.Parser

mapAppInstance :: ScheduleRunnerConfig
    -> ScheduleEntry
    -> IO ()
mapAppInstance cfg (ScheduleEntry _ (TargetSchedule tId (SchedulableApp PushTextMessage arg))) =
    flip runReaderT cfg $ do
        retryMax <- asks $ cfgAppRetryMax . cfgApp . srcCfg
        getAccessToken
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
    >>= parseSchedule
    <&> mapM_ (cronMapper cfg)
    where
        cronMapper srCfg = uncurry C.addJob . (mapAppInstance cfg &&& (C.serializeCronSchedule . seSchedule))

runSchedule :: (MonadThrow m, MonadIO m)
    => P.SomeBase P.File
    -> ScheduleRunnerConfig
    -> m [ThreadId]
runSchedule fp cfg = readSchedule fp cfg
    >>= liftIO . C.execSchedule

withScheduleRef :: P.SomeBase P.File
    -> ScheduleRunnerConfig
    -> (IORef [ThreadId] -> IO a)
    -> IO a
withScheduleRef fp cfg = bracket
    (runSchedule fp cfg >>= newIORef)
    (readIORef >=> mapM_ killThread)

watchSchedule :: (MonadThrow m, MonadIO m)
    => Bool
    -> P.SomeBase P.File
    -> LBKiirotoriConfig
    -> m ()
watchSchedule qFlag fp cfg = liftIO $ do
    unless qFlag (putStrLn "ready to boot scheduler")
    withScheduleRunner cfg $ \srCfg ->
        withScheduleRef fp srCfg $ \tIdsRef -> do
            modTime <- getCronModificationTime >>= newMVar
            withManager $ \mgr -> do
                watchDir mgr (P.fromSomeDir $ mapSomeBase P.parent fp) predicate $ \event -> do
                    mt <- getCronModificationTime
                    ifM ((== mt) <$> takeMVar modTime) (putMVar modTime mt) $
                        unless qFlag (putUpdateLog event)
                            >> putMVar modTime mt
                            >> readIORef tIdsRef
                            >>= mapM_ killThread
                            >> runSchedule fp srCfg
                            >>= writeIORef tIdsRef
                forever $ threadDelay 1000000
    where
        predicate (Modified ufp _ False) = Just fp == P.parseSomeFile ufp
        predicate _                      = False

        putUpdateLog = putStrLn
            . printf "update detected of cron file (%s), reloading"
            . eventPath

        getCronModificationTime = prjSomeBaseM P.getModificationTime fp
