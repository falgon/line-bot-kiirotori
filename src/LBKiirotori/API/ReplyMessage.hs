{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.API.ReplyMessage (
    ReplyMessage (..)
  , replyMessage
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
import           LBKiirotori.Internal.Utils       (stripFirstToLowerLabeledOption)

data ReplyMessage = ReplyMessage {
    replyMessageReplyToken           :: T.Text
  , replyMessageMessages             :: Messages
  , replyMessageNotificationDisabled :: Maybe Bool
  } deriving (Eq, Show, Generic)

instance ToJSON ReplyMessage where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 12

reqReplyMessage :: B.ByteString -> ReplyMessage ->  Request
reqReplyMessage = reqMessage "https://api.line.me/v2/bot/message/reply"

replyMessage :: (MonadThrow m, MonadIO m) => AccessToken -> ReplyMessage -> m ()
replyMessage token pm = do
    resp <- httpLbs (reqReplyMessage (atToken token) pm)
    if getResponseStatusCode resp == 200 then
        pure ()
    else
        throwString ||| throw
            $ (eitherDecode (getResponseBody resp) :: Either String MessageErrorResp)
