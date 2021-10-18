{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.API.PushMessage (
    pushMessage
) where

import           Control.Arrow                 ((|||))
import           Control.Exception.Safe        (Exception, MonadThrow (..),
                                                throw, throwString)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import           Network.HTTP.Conduit          (RequestBody (..),
                                                requestHeaders)
import           Network.HTTP.Simple

import           LBKiirotori.AccessToken.Redis (AccessToken (..))
import           LBKiirotori.Data.PushMessage

linePushMessageEndPoint :: String
linePushMessageEndPoint = "https://api.line.me/v2/bot/message/push"

buildBearerAuth :: B.ByteString -> B.ByteString
buildBearerAuth = B.append "Bearer "

applyBearerAuth :: B.ByteString -> Request -> Request
applyBearerAuth bearerToken req = setRequestHeaders (authHeader : requestHeaders req) req
    where
        authHeader = ("Authorization", buildBearerAuth bearerToken)

setRequestBearerAuth :: B.ByteString -> Request -> Request
setRequestBearerAuth = applyBearerAuth

reqPushMessage :: B.ByteString -> PushMessage ->  Request
reqPushMessage token pm = setRequestMethod "POST"
    $ setRequestBearerAuth token
    $ setRequestBodyJSON pm
    $ parseRequest_ linePushMessageEndPoint

newtype PushMessageErrorResp = PushMessageErrorResp {
    pushMessageErr :: T.Text
  } deriving Show

instance FromJSON PushMessageErrorResp where
    parseJSON (Object v) = PushMessageErrorResp
        <$> v .: "message"
    parseJSON invalid = prependFailure "parsing PushMessageErrorResp failed, "
        $ typeMismatch "Object" invalid

instance Exception PushMessageErrorResp

pushMessage :: (MonadThrow m, MonadIO m) => AccessToken -> PushMessage -> m ()
pushMessage token pm = do
    resp <- httpLbs (reqPushMessage (atToken token) pm)
    if getResponseStatusCode resp == 200 then
        pure ()
    else
        throwString ||| throw
            $ (eitherDecode (getResponseBody resp) :: Either String PushMessageErrorResp)
