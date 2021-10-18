{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
module LBKiirotori.AccessToken.Redis (
    newConn
  , writeToken
  , takeValidToken
) where

import           Control.Arrow                ((|||))
import           Control.Exception.Safe       (MonadThrow (..), throw,
                                               throwString)
import           Control.Monad                (void)
import           Control.Monad.Extra          (ifM)
import           Control.Monad.IO.Class       (MonadIO (..))
import qualified Data.ByteString.UTF8         as BS
import           Data.Maybe                   (catMaybes)
import           Data.String                  (IsString (..))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time.Clock              (UTCTime, addUTCTime,
                                               getCurrentTime)
import           Data.Time.Clock.POSIX        (posixSecondsToUTCTime,
                                               utcTimeToPOSIXSeconds)
import           Database.Redis               (Connection, checkedConnect,
                                               defaultConnectInfo, hmget, hmset,
                                               runRedis)
import           Text.Read                    (readEither)

import           LBKiirotori.AccessToken.Core (LineIssueChannelResp (..))

import           Data.Time.Format             (defaultTimeLocale, formatTime,
                                               rfc822DateFormat)
import           Data.Time.LocalTime          (TimeZone (..),
                                               getCurrentTimeZone,
                                               utcToLocalTime)

newConn :: MonadIO m => m Connection
newConn = liftIO $ checkedConnect defaultConnectInfo

writeToken :: MonadIO m
    => Connection
    -> UTCTime
    -> LineIssueChannelResp
    -> m ()
writeToken conn currentTime reqResp = liftIO $ runRedis conn $ do
    z <- liftIO getCurrentTimeZone
    liftIO $ putStrLn
        $ mappend "register token expired at: "
        $ formatTime defaultTimeLocale rfc822DateFormat
        $ utcToLocalTime z expiredTime
    void $ hmset "tokens" [
        ("expiredtime", fromString $ show val)
      , ("token", fromString $ T.unpack $ accessToken reqResp)
      ]
    where
        expiredTime = addUTCTime (fromIntegral $ expiresIn reqResp) currentTime
        val = realToFrac $ utcTimeToPOSIXSeconds expiredTime

takeToken :: (MonadThrow m, MonadIO m)
    => Connection
    -> m (Maybe (UTCTime, BS.ByteString))
takeToken conn = liftIO $ runRedis conn $ do
    x <- hmget "tokens" ["expiredtime", "token"] >>= fail . show ||| pure
    if length x == 2 then let x' = catMaybes x in
        if length x' == 2 then
            Just . (, x' !! 1) . doubleToUTCTime <$> (fail ||| pure) (readEither $ BS.toString $ head x')
        else if null x' then
            pure Nothing
        else corruptFail
    else corruptFail
    where
        doubleToUTCTime :: Double -> UTCTime
        doubleToUTCTime = posixSecondsToUTCTime . realToFrac
        corruptFail = fail "unexpected take token: maybe data is corrupt ?"

takeValidToken :: (MonadThrow m, MonadIO m)
    => Connection
    -> m (Maybe BS.ByteString)
takeValidToken conn = takeToken conn >>= \case
    Nothing -> pure Nothing
    Just (et, tk) -> ifM ((<) <$> liftIO getCurrentTime <*> pure et)
        (pure $ Just tk)
      $ pure Nothing

