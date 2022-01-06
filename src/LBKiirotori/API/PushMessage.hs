{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.API.PushMessage (
    pushMessage
) where

import           Control.Arrow                    ((&&&), (|||))
import           Control.Exception.Safe           (Exception, MonadThrow (..),
                                                   throw, throwString)
import           Control.Monad                    (unless)
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
import           LBKiirotori.Internal.HTTP        (reqMessage)
import           LBKiirotori.Internal.Utils       (stripFirstToLowerLabeledOption)

reqPushMessage :: B.ByteString -> PushMessage ->  Request
reqPushMessage = reqMessage "https://api.line.me/v2/bot/message/push"

data PushMessage = PushMessage {
    pmTo       :: T.Text
  , pmMessages :: Messages
  } deriving (Show, Generic)

instance ToJSON PushMessage where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 2

pushMessage :: (MonadThrow m, MonadIO m) => AccessToken -> PushMessage -> m ()
pushMessage token pm = do
    (statusCode, body) <- (getResponseStatusCode &&& getResponseBody)
        <$> httpLbs (reqPushMessage (atToken token) pm)
    unless (statusCode == 200) $
        throwString ||| throw
            $ (eitherDecode body :: Either String MessageErrorResp)
