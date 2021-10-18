{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.AccessToken.JWT (
    getJwt
  , newClaimsSet
) where

import           Control.Applicative             (liftA3)
import           Control.Arrow                   ((|||))
import           Control.Exception.Safe          (MonadThrow (..), throw,
                                                  throwString)
import           Control.Lens.Operators          ((&), (.~), (?~))
import           Control.Monad.Error.Class       (MonadError)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Parallel          (MonadParallel, bindM3)
import           Crypto.JWT
import           Data.Aeson                      (Value (..), eitherDecode,
                                                  encode)
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.UTF8       as BLU
import qualified Data.HashMap.Strict             as HM
import           Data.Scientific                 (Scientific)
import           Data.String                     (IsString (..))
import qualified Data.Text                       as T
import           Data.Time.Clock                 (addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX           (utcTimeToPOSIXSeconds)
import           Data.Word                       (Word8)
import           Prelude                         hiding (exp)
import           System.Environment              (getEnv)

import           LBKiirotori.AccessToken.Config  (getChannelID)
import           LBKiirotori.Internal.Exceptions (invalidArgument)

getJwk :: (MonadThrow m, MonadIO m) => m JWK
getJwk = liftIO (getEnv jwkKeyEnv)
    >>= (throwString ||| pure) . eitherDecode . BLU.fromString
    where
        jwkKeyEnv = "LINE_BOT_JWK_SET"

newClaimsSet :: (MonadThrow m, MonadIO m) => Scientific -> m ClaimsSet
newClaimsSet seconds
    | seconds <= tokenExpMax  = do
        channelID <- getChannelID
        expDate <- NumericDate . addUTCTime jwtAssertionMax <$> liftIO getCurrentTime
        pure $ emptyClaimsSet
            & claimIss ?~ channelID
            & claimSub ?~ channelID
            & claimAud ?~ Audience ["https://api.line.me/"]
            & claimExp ?~ expDate
            & unregisteredClaims .~ HM.singleton "token_exp" (Number seconds)
    where
        jwtAssertionMax = 30 * 60
        tokenExpMax = 60 * 60 * 24 * 30 -- The max lifetime of a channel access token is 30 days.

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
getJwt :: (AsError e, MonadParallel m, MonadThrow m, MonadRandom m, MonadIO m, MonadError e m)
    => Scientific
    -> m SignedJWT
getJwt secondsLim = bindM3 signClaims getJwk newJwtHeader (newClaimsSet secondsLim)
