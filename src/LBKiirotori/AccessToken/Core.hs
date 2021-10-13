{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.AccessToken.Core (
    LineReqResp
  , reqAccessToken
) where

import           LBKiirotori.AccessToken.JWT (getJwt)

import           Control.Arrow               ((|||))
import           Control.Exception.Safe      (MonadThrow (..), throw,
                                              throwString)
import           Control.Monad.Error.Class   (MonadError)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Crypto.JWT                  (AsError, MonadRandom, SignedJWT,
                                              encodeCompact)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T
import           Data.Word                   (Word32)
import           Network.HTTP.Simple

data LineReqResp = LineReqResp {
    accessToken :: T.Text
  , tokenType   :: T.Text
  , expiresIn   :: Word32
  , keyID       :: T.Text
  } deriving Show

data LineReqErrResp = LineReqErrResp {
    lineReqError            :: String
  , lineReqErrorDescription :: String
  } deriving Show

instance FromJSON LineReqResp where
    parseJSON (Object v) = LineReqResp
        <$> v .: "access_token"
        <*> v .: "token_type"
        <*> v .: "expires_in"
        <*> v .: "key_id"
    parseJSON invalid = prependFailure "parsing LineReqResp failed, "
        $ typeMismatch "Object" invalid

instance FromJSON LineReqErrResp where
    parseJSON (Object v) = LineReqErrResp
        <$> v .: "error"
        <*> v .: "error_description"
    parseJSON invalid = prependFailure "parsing LineReqErrResp failed, "
        $ typeMismatch "Object" invalid

lineReqEndPoint :: String
lineReqEndPoint = "https://api.line.me/oauth2/v2.1/token"

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#issue-channel-access-token-v2-1
reqParam :: SignedJWT -> Request
reqParam jwt = setRequestBodyURLEncoded dataURLEncode
    $ parseRequest_ lineReqEndPoint
    where
        dataURLEncode = [
            ("grant_type", "client_credentials")
          , ("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
          , ("client_assertion", BL.toStrict $ encodeCompact jwt)
          ]

postHTTPLBS :: (MonadThrow m, MonadIO m) => Request -> m LineReqResp
postHTTPLBS req = do
    resp <- liftIO $ httpLBS req
    if getResponseStatusCode resp == 200 then
        throwString ||| pure $ eitherDecode $ getResponseBody resp
    else
        (throwString ||| pure $ eitherDecode $ getResponseBody resp)
            >>= throwString . (show :: LineReqErrResp -> String)

reqAccessToken :: (AsError e, MonadThrow m, MonadRandom m, MonadIO m, MonadError e m) => m LineReqResp
reqAccessToken = getJwt 10 >>= postHTTPLBS . reqParam
