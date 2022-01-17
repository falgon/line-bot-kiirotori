{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module LBKiirotori.AccessToken.JWT (
    getJwt
) where

import           Control.Arrow                   ((|||))
import           Control.Exception.Safe          (MonadThrow (..), throw,
                                                  throwString)
import           Control.Lens.Operators          ((&), (.~), (?~))
import           Control.Monad.Error.Class       (MonadError)
import           Control.Monad.Except            (ExceptT, runExceptT)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Parallel          (MonadParallel, bindM3)
import           Crypto.JWT                      hiding (sign)
import           Data.Aeson                      (Value (..), eitherDecode,
                                                  encode)
import qualified Data.ByteString.Char8           as BC
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.UTF8       as BLU
import qualified Data.HashMap.Strict             as HM
import           Data.Scientific                 (Scientific)
import           Data.String                     (IsString (..))
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Data.Time.Clock                 (addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX           (utcTimeToPOSIXSeconds)
import           Data.Word                       (Word8)
import           Prelude                         hiding (exp)

import           LBKiirotori.AccessToken.Class   (AccessTokenMonad (..))
import           LBKiirotori.Internal.Exceptions (invalidArgument,
                                                  stringException)
import           LBKiirotori.Internal.Utils      (decodeJSON)

getJwk :: (MonadThrow m, AccessTokenMonad m) => m JWK
getJwk = lineJWKSet >>= decodeJSON . BL.fromStrict

newClaimsSet :: (MonadIO m, AccessTokenMonad m)
    => Scientific
    -> m ClaimsSet
newClaimsSet seconds
    | seconds <= tokenExpMax = do
        channelID <- fromString . BC.unpack <$> lineChanId
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

newJwtHeader :: AccessTokenMonad m
    => m (JWSHeader ())
newJwtHeader = do
    kidVal <- T.decodeUtf8 <$> lineKId
    pure $ newJWSHeader ((), RS256)
        & kid ?~ HeaderParam () kidVal
        & typ ?~ HeaderParam () "JWT"

sign :: forall m. (MonadIO m, MonadThrow m)
    => JWK
    -> JWSHeader ()
    -> ClaimsSet
    -> m SignedJWT
sign jwkSet header claimsSet = liftIO (runExceptT $ sign' jwkSet header claimsSet)
    >>= throw ||| pure
    where
        sign' :: JWK
            -> JWSHeader ()
            -> ClaimsSet
            -> ExceptT JWTError IO SignedJWT
        sign' = signClaims

-- | Generate channel access tokens v2.1
-- c.f. https://developers.line.biz/en/docs/messaging-api/generate-json-web-token/#generate-jwt
getJwt :: (MonadIO m, MonadThrow m, MonadParallel m, AccessTokenMonad m)
    => Scientific
    -> m SignedJWT
getJwt = bindM3 sign getJwk newJwtHeader . newClaimsSet

