module LBKiirotori.Internal.Cron (
    execScheduleWithLocalTime
) where

import           Control.Arrow                 ((|||))
import           Control.Concurrent            (ThreadId, forkIO, threadDelay)
import           Control.Exception.Safe        (MonadThrow, throwString)
import           Control.Monad                 (forever, void, when)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Time.Clock               (UTCTime)
import           Data.Time.LocalTime           (getCurrentTimeZone,
                                                localTimeToUTC)
import           System.Cron.Internal.Check    (scheduleMatches)
import           System.Cron.Internal.Schedule (findNextMinuteDelay')
import           System.Cron.Schedule          (Job (..), Schedule, runSchedule)

import           LBKiirotori.Internal.Utils    (getCurrentLocalTime)

findNextMinuteDelay :: MonadIO m => m (UTCTime, Int)
findNextMinuteDelay = (.) findNextMinuteDelay' . localTimeToUTC
    <$> liftIO getCurrentTimeZone <*> getCurrentLocalTime

forkJob :: MonadIO m
    => Job
    -> m ThreadId
forkJob (Job s a) = liftIO $ forkIO $ forever $ do
    (timeAt, delay) <- findNextMinuteDelay
    threadDelay delay >> when (scheduleMatches s timeAt) (void $ forkIO a)

execScheduleWithLocalTime :: (MonadIO m, MonadThrow m)
    => Schedule ()
    -> m [ThreadId]
execScheduleWithLocalTime s = ((throwString . show) ||| (mapM forkJob . snd))
    $ runSchedule s
