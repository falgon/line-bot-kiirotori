{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.AccessToken.JWT (
    getJwt
) where

import           LBKiirotori.Internal.Exceptions (invalidArgument)

import           Control.Arrow                   ((|||))
import           Control.Exception.Safe          (MonadThrow (..), throw,
                                                  throwString)
import           Control.Lens.Operators          ((&), (.~), (?~))
import           Control.Monad.Error.Class       (MonadError)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Crypto.JWT
import           Data.Aeson                      (Value (..), eitherDecode,
                                                  encode)
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.UTF8       as BLU
import qualified Data.HashMap.Strict             as HM
import           Data.String                     (IsString (..))
import qualified Data.Text                       as T
import           Data.Time.Clock                 (addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX           (utcTimeToPOSIXSeconds)
import           Data.Word                       (Word8)
import           Prelude                         hiding (exp)
import           System.Environment              (getEnv)

getJwk :: (MonadThrow m, MonadIO m) => m JWK
getJwk = liftIO (getEnv jwkKeyEnv)
    >>= (throwString ||| pure) . eitherDecode . BLU.fromString
    where
        jwkKeyEnv = "LINE_BOT_JWK_SET"

newClaimsSet :: (MonadThrow m, MonadIO m) => Word8 -> m ClaimsSet
newClaimsSet minutes
    | minutes <= 30 = do
        channelID <- fromString <$> liftIO (getEnv channelIDEnv)
        expDate <- NumericDate . addUTCTime nMinutes <$> liftIO getCurrentTime
        pure $ emptyClaimsSet
            & claimIss ?~ channelID
            & claimSub ?~ channelID
            & claimAud ?~ Audience ["https://api.line.me/"]
            & claimExp ?~ expDate
            & unregisteredClaims .~ HM.singleton "token_exp" (Number 86400)
    where
        channelIDEnv = "LINE_CHANEL_ID"
        nMinutes = fromIntegral minutes * 60 -- n minutes access limit

newJwtHeader :: MonadIO m => m (JWSHeader ())
newJwtHeader = do
    kidVal <- T.pack <$> liftIO (getEnv kidEnv)
    pure $ newJWSHeader ((), RS256)
        & kid ?~ HeaderParam () kidVal
        & typ ?~ HeaderParam () "JWT"
    where
        kidEnv = "LINE_BOT_KID"

-- | Generate channel access tokens v2.1
-- c.f. https://developers.line.biz/en/docs/messaging-api/generate-json-web-token/#generate-jwt
getJwt :: (AsError e, MonadThrow m, MonadRandom m, MonadIO m, MonadError e m) => Word8 -> m SignedJWT
getJwt minutesLim = do
    jwk <- getJwk
    p <- newClaimsSet 10
    header <- newJwtHeader
    signClaims jwk header p
