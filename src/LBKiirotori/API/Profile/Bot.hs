{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.API.Profile.Bot (
    reqProfileBot
  , ProfileBotResp (..)
) where

import           Control.Arrow                    ((&&&), (|||))
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

import           LBKiirotori.AccessToken.Config   (AccessToken (..))
import           LBKiirotori.API.MessageErrorResp
import           LBKiirotori.Data.MessageObject
import           LBKiirotori.Internal.HTTP        (reqGet)
import           LBKiirotori.Internal.Utils       (decodeJSON,
                                                   stripFirstToLowerLabeledOption)

data ProfileBotResp = ProfileBotResp {
    pmUserId         :: T.Text
  , pmBasicId        :: T.Text
  , pmPremiumId      :: Maybe T.Text
  , pmDisplayName    :: T.Text
  , pmPictureUrl     :: Maybe T.Text
  , pmChatMode       :: T.Text
  , pmMarkAsReadMode :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON ProfileBotResp where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 2

instance FromJSON ProfileBotResp where
    parseJSON (Object v) = ProfileBotResp
        <$> v .: "userId"
        <*> v .: "basicId"
        <*> v .:? "premiumId"
        <*> v .: "displayName"
        <*> v .:? "pictureUrl"
        <*> v .: "chatMode"
        <*> v .: "markAsReadMode"
    parseJSON invalid = prependFailure "parsing ProfileBotResp failed, "
        $ typeMismatch "Object" invalid

reqGetBot :: B.ByteString -> Request
reqGetBot = reqGet "https://api.line.me/v2/bot/info"

reqProfileBot :: (MonadThrow m, MonadIO m) => AccessToken -> m ProfileBotResp
reqProfileBot token = do
    (statusCode, body) <- (getResponseStatusCode &&& getResponseBody)
        <$> httpLbs (reqGetBot (atToken token))
    if statusCode == 200 then decodeJSON body
    else throwString ||| throw
        $ (eitherDecode body :: Either String MessageErrorResp)
