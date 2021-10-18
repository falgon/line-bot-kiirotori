{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.AccessToken.Core (
    LineIssueChannelResp (..)
  , LineAllValidCATKIdsResp  (..)
  , reqAccessToken
  , reqAllValidCATKIds
  , reqRevokeChannelAccess
) where

import           Control.Arrow                  ((|||))
import           Control.Exception.Safe         (MonadThrow (..), throw,
                                                 throwString)
import           Control.Monad.Error.Class      (MonadError)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Parallel         (MonadParallel)
import           Crypto.JWT                     (AsError, MonadRandom,
                                                 SignedJWT, encodeCompact)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Data.Scientific                (Scientific)
import qualified Data.Text                      as T
import           Data.Word                      (Word32)
import           Network.HTTP.Simple

import           LBKiirotori.AccessToken.Config (AccessToken (..))
import           LBKiirotori.AccessToken.JWT    (getJwt)

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

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#issue-channel-access-token-v2-1
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

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#verfiy-channel-access-token-v2-1
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

reqRevokeChannelAccessToken :: AccessToken
    -> BS.ByteString
    -> BS.ByteString
    -> Request
reqRevokeChannelAccessToken tk cID cSecret = setRequestBodyURLEncoded dataURLEncode
    $ parseRequest_ lineReqOauth2RevokeEndpoint
    where
        dataURLEncode = [
            ("client_id", cID)
          , ("client_secret", cSecret)
          , ("access_token", atToken tk)
          ]
        lineReqOauth2RevokeEndpoint = "https://api.line.me/oauth2/v2.1/revoke"

sendReq :: (MonadThrow m, MonadIO m, FromJSON a)
    => Request
    -> m a
sendReq req = do
    resp <- liftIO $ httpLBS req
    if getResponseStatusCode resp == 200 then
        throwString ||| pure $ eitherDecode $ getResponseBody resp
    else
        (throwString ||| pure $ eitherDecode $ getResponseBody resp)
            >>= throwString . (show :: LineReqErrResp -> String)

tokenLimSeconds :: Scientific
tokenLimSeconds = 10 * 60 -- 10 minutes

reqAccessToken :: (AsError e, MonadParallel m, MonadThrow m, MonadRandom m, MonadIO m, MonadError e m)
    => m LineIssueChannelResp
reqAccessToken = getJwt tokenLimSeconds >>= sendReq . reqIssueChannelParam

reqAllValidCATKIds :: (AsError e, MonadParallel m, MonadThrow m, MonadRandom m, MonadIO m, MonadError e m)
    => m LineAllValidCATKIdsResp
reqAllValidCATKIds = getJwt tokenLimSeconds >>= sendReq . reqAllValidCATKIdsParam

reqRevokeChannelAccess :: (MonadThrow m, MonadIO m)
    => AccessToken
    -> BS.ByteString
    -> BS.ByteString
    -> m ()
reqRevokeChannelAccess tk cID cSecret = do
    resp <- liftIO $ httpLBS $ reqRevokeChannelAccessToken tk cID cSecret
    if getResponseStatusCode resp == 200 then pure ()
    else
        (throwString ||| pure $ eitherDecode $ getResponseBody resp)
            >>= throwString . (show :: LineReqErrResp -> String)

