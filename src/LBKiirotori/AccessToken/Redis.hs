{-# LANGUAGE DataKinds, FlexibleContexts, LambdaCase, OverloadedStrings,
             ScopedTypeVariables #-}
module LBKiirotori.AccessToken.Redis (
    newConn
  , writeToken
  , takeToken
  , takeValidToken
  , AccessToken (..)
) where

import           Control.Applicative                            (Alternative (..))
import           Control.Arrow                                  ((|||))
import           Control.Exception.Safe                         (MonadThrow (..),
                                                                 throw,
                                                                 throwString)
import           Control.Monad                                  (void)
import           Control.Monad.Extra                            (ifM)
import           Control.Monad.IO.Class                         (MonadIO (..))
import           Control.Monad.Trans                            (lift)
import           Control.Monad.Trans.Maybe                      (runMaybeT)
import qualified Data.ByteString.UTF8                           as BS
import           Data.Functor                                   ((<&>))
import           Data.Functor                                   (($>))
import           Data.Maybe                                     (catMaybes)
import           Data.Proxy
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
import qualified Data.Vector.Fixed                              as V
import qualified Data.Vector.Fixed.Boxed                        as V
import           Database.Redis                                 (ConnectInfo,
                                                                 Connection,
                                                                 RedisCtx,
                                                                 Reply,
                                                                 checkedConnect,
                                                                 defaultConnectInfo,
                                                                 get, hmget,
                                                                 hmset)
import           Text.Read                                      (readEither)

import           LBKiirotori.AccessToken.Config                 (AccessToken (..))
import           LBKiirotori.AccessToken.Core                   (LineIssueChannelResp (..))
import           LBKiirotori.Internal.Utils                     (hoistMaybe)
import           LBKiirotori.Webhook.EventObject.LineBotHandler (LineBotHandler,
                                                                 runRedis)

newConn :: MonadIO m => ConnectInfo -> m Connection
newConn = liftIO . checkedConnect

writeToken :: UTCTime
    -> LineIssueChannelResp
    -> LineBotHandler AccessToken
writeToken currentTime reqResp = runRedis $
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

eitherFail :: (MonadFail m, Show a) => Either a b -> m b
eitherFail = fail . show ||| pure

hmget' :: forall m v.
    (MonadFail m, RedisCtx m (Either Reply), V.Vector v BS.ByteString, V.Vector v (Maybe BS.ByteString))
    => BS.ByteString
    -> v BS.ByteString
    -> m (Maybe (v BS.ByteString))
hmget' key field = do
    vs <- hmget key (V.toList field)
        >>= eitherFail
        >>= maybe (fail $ mconcat [
            "unexpexed to take token: expected "
          , show (V.length field)
          , " results"
          ]) (pure . catMaybes . V.toList) . toFixed
    pure $ if null vs then empty else toFixed' vs
    where
        toFixed = V.fromListM :: [Maybe BS.ByteString] -> Maybe (v (Maybe BS.ByteString))
        toFixed' = V.fromListM :: [BS.ByteString] -> Maybe (v (BS.ByteString))

takeToken :: LineBotHandler (Maybe AccessToken)
takeToken = runMaybeT $ do
    res <- lift (runRedis $ hmget' "tokens" $ mk3Vec "expiredtime" "token" "kid")
        >>= hoistMaybe
    AccessToken (T.decodeUtf8 (res `V.index` two)) (res `V.index` one) . doubleToUTCTime
        <$> ((lift . throwString) ||| (lift . pure)) (readEither (BS.toString (res `V.index` zero)))
    where
        doubleToUTCTime :: Double -> UTCTime
        doubleToUTCTime = posixSecondsToUTCTime . realToFrac

        mk3Vec :: a
            -> a
            -> a
            -> V.Vec3 a
        mk3Vec = V.mk3

        zero = Proxy :: Proxy 0
        one = Proxy :: Proxy 1
        two = Proxy :: Proxy 2

takeValidToken :: LineBotHandler (Maybe AccessToken)
takeValidToken = runMaybeT $ do
    tk <- lift takeToken >>= hoistMaybe
    ifM ((<) <$> liftIO getCurrentTime <*> pure (atExpiresIn tk))
        (pure tk)
        empty

getPinCode :: IsString s => LineBotHandler s
getPinCode = runRedis (get pinCodeKey >>= eitherFail)
    >>= maybe (throwString "cannot get PINCODE") (pure . fromString . BS.toString)
    where
        pinCodeKey = "PINCODE"
