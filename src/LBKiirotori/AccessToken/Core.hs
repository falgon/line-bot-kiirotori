{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module LBKiirotori.AccessToken.Core (
    LineIssueChannelResp (..)
  , LineAllValidCATKIdsResp  (..)
  , reqAccessToken
  , reqAllValidCATKIds
  , reqRevokeChannelAccess
) where

import           Control.Arrow                  ((|||))
import           Control.Exception.Safe         (Exception, MonadThrow (..),
                                                 throw, throwString)
import           Control.Monad                  (liftM2)
import           Control.Monad.Error.Class      (MonadError)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Parallel         (MonadParallel)
import           Crypto.JWT                     (AsError, MonadRandom,
                                                 SignedJWT, encodeCompact)
import           Data.Aeson.Types
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Data.Functor                   ((<&>))
import           Data.Scientific                (Scientific)
import qualified Data.Text                      as T
import           Data.Word                      (Word32)
import           Network.HTTP.Simple

import           LBKiirotori.AccessToken.Class  (AccessTokenMonad (..))
import           LBKiirotori.AccessToken.Config (AccessToken (..))
import           LBKiirotori.AccessToken.JWT    (getJwt)
import           LBKiirotori.Internal.Utils     (decodeJSON)

data LineIssueChannelResp = LineIssueChannelResp {
    accessToken :: T.Text
  , tokenType   :: T.Text
  , expiresIn   :: Word32
  , keyID       :: T.Text
  } deriving Show

newtype LineAllValidCATKIdsResp = LineAllValidCATKIdsResp {
    verifiedKIds :: [T.Text]
  } deriving Show

data LineReqErrResp = LineReqErrResp {
    lineReqError            :: String
  , lineReqErrorDescription :: String
  } deriving Show

instance Exception LineReqErrResp

instance FromJSON LineIssueChannelResp where
    parseJSON (Object v) = LineIssueChannelResp
        <$> v .: "access_token"
        <*> v .: "token_type"
        <*> v .: "expires_in"
        <*> v .: "key_id"
    parseJSON invalid = prependFailure "parsing LineIssueChannelResp failed, "
        $ typeMismatch "Object" invalid

instance FromJSON LineAllValidCATKIdsResp where
    parseJSON (Object v) = LineAllValidCATKIdsResp
        <$> v .: "kids"
    parseJSON invalid = prependFailure "parsing LineAllValidCATKIdsResp failed, "
        $ typeMismatch "Object" invalid

instance FromJSON LineReqErrResp where
    parseJSON (Object v) = LineReqErrResp
        <$> v .: "error"
        <*> v .: "error_description"
    parseJSON invalid = prependFailure "parsing LineReqErrResp failed, "
        $ typeMismatch "Object" invalid

-- c.f. https://developers.line.biz/en/reference/messaging-api/#issue-channel-access-token-v2-1
reqIssueChannelParam :: SignedJWT -> Request
reqIssueChannelParam jwt = setRequestBodyURLEncoded dataURLEncode
    $ parseRequest_ lineReqOauth2TokenEndpoint
    where
        dataURLEncode = [
            ("grant_type", "client_credentials")
          , ("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
          , ("client_assertion", BL.toStrict $ encodeCompact jwt)
          ]
        lineReqOauth2TokenEndpoint = "https://api.line.me/oauth2/v2.1/token"

-- c.f. https://developers.line.biz/en/reference/messaging-api/#verfiy-channel-access-token-v2-1
reqAllValidCATKIdsParam :: SignedJWT -> Request
reqAllValidCATKIdsParam jwt = setRequestMethod "GET"
    $ setRequestQueryString q
    $ parseRequest_ lineReqOauth2ValidTokenEndpoint
    where
        q = [
            ("client_assertion_type", Just "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
          , ("client_assertion", Just $ BL.toStrict $ encodeCompact jwt)
          ]
        lineReqOauth2ValidTokenEndpoint = "https://api.line.me/oauth2/v2.1/tokens/kid"

-- c.f. https://developers.line.biz/en/reference/messaging-api/#revoke-channel-access-token-v2-1
reqRevokeChannelAccessToken :: AccessTokenMonad m => AccessToken -> m Request
reqRevokeChannelAccessToken tk = liftM2 dataURLEncode lineChanId lineChanSecret
    <&> flip setRequestBodyURLEncoded (parseRequest_ lineReqOauth2RevokeEndpoint)
    where
        dataURLEncode cID cSecret = [
            ("client_id", cID)
          , ("client_secret", cSecret)
          , ("access_token", atToken tk)
          ]
        lineReqOauth2RevokeEndpoint = "https://api.line.me/oauth2/v2.1/revoke"

sendReq :: forall m a. (MonadThrow m, MonadIO m, FromJSON a)
    => Request
    -> m a
sendReq req = do
    resp <- httpLBS req
    if getResponseStatusCode resp == 200 then decodeJSON $ getResponseBody resp
    else decodeJSON (getResponseBody resp) >>= (throw :: LineReqErrResp -> m a)

tokenLimSeconds :: Scientific
tokenLimSeconds = 10 * 60 -- 10 minutes

reqAccessToken :: (MonadIO m, MonadThrow m, MonadParallel m, AccessTokenMonad m)
    => m LineIssueChannelResp
reqAccessToken = getJwt tokenLimSeconds >>= sendReq . reqIssueChannelParam

reqAllValidCATKIds :: (MonadIO m, MonadThrow m, MonadParallel m, AccessTokenMonad m)
    => m LineAllValidCATKIdsResp
reqAllValidCATKIds = getJwt tokenLimSeconds >>= sendReq . reqAllValidCATKIdsParam

reqRevokeChannelAccess :: forall m. (MonadIO m, MonadThrow m, AccessTokenMonad m)
    => AccessToken
    -> m ()
reqRevokeChannelAccess tk = do
    resp <- reqRevokeChannelAccessToken tk >>= httpLBS
    if getResponseStatusCode resp == 200 then pure ()
    else decodeJSON (getResponseBody resp) >>= (throw :: LineReqErrResp -> m ())

