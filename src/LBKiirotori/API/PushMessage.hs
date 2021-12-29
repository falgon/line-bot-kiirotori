{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.API.PushMessage (
    pushMessage
) where

import           Control.Arrow                    ((|||))
import           Control.Exception.Safe           (Exception, MonadThrow (..),
                                                   throw, throwString)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString                  as B
import qualified Data.Text                        as T
import           GHC.Generics
import           Network.HTTP.Conduit             (RequestBody (..),
                                                   requestHeaders)
import           Network.HTTP.Simple

import           LBKiirotori.API.MessageErrorResp
import           LBKiirotori.Data.MessageObject
import           LBKiirotori.Database.Redis       (AccessToken (..))
import           LBKiirotori.Internal.HTTP

reqPushMessage :: B.ByteString -> PushMessage ->  Request
reqPushMessage = reqMessage "https://api.line.me/v2/bot/message/push"

data PushMessage = PushMessage {
    to       :: T.Text
  , messages :: Messages
  } deriving (Show, Generic)

instance ToJSON PushMessage

pushMessage :: (MonadThrow m, MonadIO m) => AccessToken -> PushMessage -> m ()
pushMessage token pm = do
    resp <- httpLbs (reqPushMessage (atToken token) pm)
    if getResponseStatusCode resp == 200 then
        pure ()
    else
        throwString ||| throw
            $ (eitherDecode (getResponseBody resp) :: Either String MessageErrorResp)
