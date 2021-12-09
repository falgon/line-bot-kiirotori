{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module LBKiirotori.AccessToken.Redis (
    newConn
  , writeToken
  , takeToken
  , takeValidToken
  , AccessToken (..)
) where

import           Control.Arrow                                  ((|||))
import           Control.Exception.Safe                         (MonadThrow (..),
                                                                 throw,
                                                                 throwString)
import           Control.Monad                                  (void)
import           Control.Monad.Extra                            (ifM)
import           Control.Monad.IO.Class                         (MonadIO (..))
import qualified Data.ByteString.UTF8                           as BS
import           Data.Functor                                   (($>))
import           Data.Maybe                                     (catMaybes)
import           Data.String                                    (IsString (..))
import qualified Data.Text                                      as T
import qualified Data.Text.Encoding                             as T
import           Data.Time.Clock                                (UTCTime,
                                                                 addUTCTime,
                                                                 getCurrentTime)
import           Data.Time.Clock.POSIX                          (posixSecondsToUTCTime,
                                                                 utcTimeToPOSIXSeconds)
import           Data.Time.Format                               (defaultTimeLocale,
                                                                 formatTime,
                                                                 rfc822DateFormat)
import           Data.Time.LocalTime                            (TimeZone (..), getCurrentTimeZone,
                                                                 utcToLocalTime)
import           Database.Redis                                 (Connection,
                                                                 checkedConnect,
                                                                 defaultConnectInfo,
                                                                 hmget, hmset,
                                                                 runRedis)
import           Text.Read                                      (readEither)

import           LBKiirotori.AccessToken.Config                 (AccessToken (..))
import           LBKiirotori.AccessToken.Core                   (LineIssueChannelResp (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler (LineBotHandler,
                                                                 askRedisConn)

newConn :: MonadIO m => m Connection
newConn = liftIO $ checkedConnect defaultConnectInfo

writeToken :: UTCTime
    -> LineIssueChannelResp
    -> LineBotHandler AccessToken
writeToken currentTime reqResp = do
    conn <- askRedisConn
    liftIO $ runRedis conn $
        liftIO getCurrentTimeZone
            >>= liftIO . putStrLn
                . mappend "Generated a new token that is expired at: "
                . formatTime defaultTimeLocale rfc822DateFormat
                . flip utcToLocalTime expiredTime
            >> void (hmset "tokens" [
                ("expiredtime", fromString $ show val)
              , ("token", fromString $ T.unpack $ accessToken reqResp)
              , ("kid", fromString $ T.unpack $ keyID reqResp)
              ])
            $> AccessToken {
                atKeyId = keyID reqResp
              , atToken = fromString $ T.unpack $ accessToken reqResp
              , atExpiresIn = expiredTime
              }
    where
        expiredTime = addUTCTime (fromIntegral $ expiresIn reqResp) currentTime
        val = realToFrac $ utcTimeToPOSIXSeconds expiredTime

takeToken :: LineBotHandler (Maybe AccessToken)
takeToken = do
    conn <- askRedisConn
    liftIO $ runRedis conn $ do
        x <- hmget "tokens" ["expiredtime", "token", "kid"] >>= fail . show ||| pure
        if length x == 3 then let x' = catMaybes x in
            if length x' == 3 then
                Just . AccessToken (T.decodeUtf8 (x' !! 2)) (x' !! 1) . doubleToUTCTime
                    <$> (fail ||| pure) (readEither $ BS.toString $ head x')
            else if null x' then
                pure Nothing
            else corruptFail
        else corruptFail
    where
        doubleToUTCTime :: Double -> UTCTime
        doubleToUTCTime = posixSecondsToUTCTime . realToFrac
        corruptFail = fail "unexpected take token: maybe data is corrupt ?"

takeValidToken :: LineBotHandler (Maybe AccessToken)
takeValidToken = takeToken >>= \case
    Nothing -> pure Nothing
    Just tk -> ifM ((<) <$> liftIO getCurrentTime <*> pure (atExpiresIn tk))
        (pure $ Just tk)
      $ pure Nothing

