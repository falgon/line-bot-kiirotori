{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings,
             ScopedTypeVariables #-}
module LBKiirotori.Database.Redis.Core (
    newConn
  , hmget'
  , hmset'
  , writeToken
  , writeBotUserId
  , takeToken
  , takeValidToken
  , AccessToken (..)
  , getAuthCode
  , takeBotUserId
) where

import           Control.Applicative             (Alternative (..))
import           Control.Arrow                   ((|||))
import           Control.Exception.Safe          (MonadThrow (..), throw,
                                                  throwString)
import           Control.Monad                   (void)
import           Control.Monad.Extra             (ifM)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans             (lift)
import           Control.Monad.Trans.Maybe       (runMaybeT)
import qualified Data.ByteString.UTF8            as BS
import           Data.Functor                    (($>), (<&>))
import           Data.Maybe                      (catMaybes)
import           Data.Proxy
import           Data.String                     (IsString (..))
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Data.Time.Clock                 (UTCTime, addUTCTime,
                                                  getCurrentTime)
import           Data.Time.Clock.POSIX           (utcTimeToPOSIXSeconds)
import           Data.Time.Format                (defaultTimeLocale, formatTime,
                                                  rfc822DateFormat)
import           Data.Time.LocalTime             (LocalTime)
import qualified Data.Vector.Fixed               as V
import qualified Data.Vector.Fixed.Boxed         as V
import           Database.Redis                  (ConnectInfo, Connection,
                                                  RedisCtx, Reply, Status,
                                                  checkedConnect,
                                                  defaultConnectInfo, get,
                                                  hmget, hmset, set)
import           Text.Read                       (readEither)

import           LBKiirotori.AccessToken.Class   (AccessTokenMonad (..))
import           LBKiirotori.AccessToken.Config  (AccessToken (..))
import           LBKiirotori.AccessToken.Core    (LineIssueChannelResp (..))
import           LBKiirotori.API.Profile.Bot     (ProfileBotResp (..))
import           LBKiirotori.Database.Redis.Keys
import           LBKiirotori.Internal.Utils      (doubleToUTCTime, hoistMaybe,
                                                  localTimeToCurrentUTCTimeZone,
                                                  utcToCurrentLocalTimeZone)

newConn :: MonadIO m => ConnectInfo -> m Connection
newConn = liftIO . checkedConnect

writeToken :: AccessTokenMonad m
    => UTCTime
    -> LineIssueChannelResp
    -> m AccessToken
writeToken currentTime reqResp = redis $
    utcToCurrentLocalTimeZone expiredTime
        >>= liftIO . putStrLn
            . mappend "Generated a new token that is expired at: "
            . formatTime defaultTimeLocale rfc822DateFormat
        >> hmset "tokens" [
            ("expiredtime", fromString $ show val)
          , ("token", fromString $ T.unpack $ accessToken reqResp)
          , ("kid", fromString $ T.unpack $ keyID reqResp)
          ]
        $> AccessToken {
            atKeyId = keyID reqResp
          , atToken = fromString $ T.unpack $ accessToken reqResp
          , atExpiresIn = expiredTime
          }
    where
        expiredTime = addUTCTime (fromIntegral $ expiresIn reqResp) currentTime
        val = realToFrac $ utcTimeToPOSIXSeconds expiredTime

writeBotUserId :: AccessTokenMonad m
    => ProfileBotResp
    -> m T.Text
writeBotUserId reqResp = let userId = pmUserId reqResp in redis $ do
    liftIO (putStrLn "Writing my id")
        >> set botUserIdKey (T.encodeUtf8 userId)
        >> pure userId

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
            "error: expected "
          , show (V.length field)
          , " results"
          ]) (pure . catMaybes . V.toList) . toFixed
    pure $ if null vs then empty else toFixed' vs
    where
        toFixed = V.fromListM :: [Maybe BS.ByteString] -> Maybe (v (Maybe BS.ByteString))
        toFixed' = V.fromListM :: [BS.ByteString] -> Maybe (v BS.ByteString)

hmset' :: (MonadFail m, RedisCtx m (Either Reply))
    => BS.ByteString
    -> [(BS.ByteString, BS.ByteString)]
    -> m Status
hmset' key field = hmset key field
    >>= eitherFail

takeToken :: (MonadThrow m, AccessTokenMonad m)
    => m (Maybe AccessToken)
takeToken = runMaybeT $ do
    res <- lift (redis $ hmget' "tokens" $ mk3Vec "expiredtime" "token" "kid")
        >>= hoistMaybe
    AccessToken (T.decodeUtf8 (res `V.index` two)) (res `V.index` one) . doubleToUTCTime
        <$> ((lift . throwString) ||| (lift . pure)) (readEither (BS.toString (res `V.index` zero)))
    where
        mk3Vec :: a
            -> a
            -> a
            -> V.Vec3 a
        mk3Vec = V.mk3

        zero = Proxy :: Proxy 0
        one = Proxy :: Proxy 1
        two = Proxy :: Proxy 2

takeValidToken :: (MonadIO m, MonadThrow m, AccessTokenMonad m)
    => m (Maybe AccessToken)
takeValidToken = runMaybeT $ do
    tk <- lift takeToken >>= hoistMaybe
    ifM ((<) <$> liftIO getCurrentTime <*> pure (atExpiresIn tk))
        (pure tk)
        empty

getAuthCode :: (IsString s, MonadThrow m, AccessTokenMonad m)
    => m s
getAuthCode = redis (get pinCodeKey >>= eitherFail)
    >>= maybe (throwString "cannot get PINCODE") (pure . fromString . BS.toString)

takeBotUserId :: (IsString s, MonadThrow m, AccessTokenMonad m)
    => m (Maybe s)
takeBotUserId = runMaybeT $
    lift (redis (get botUserIdKey >>= eitherFail))
        >>= hoistMaybe
        <&> fromString . BS.toString
