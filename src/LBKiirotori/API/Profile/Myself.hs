{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.API.Profile.Myself (
    profileMyself
  , ProfileMyselfResp (..)
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

import           LBKiirotori.API.MessageErrorResp
import           LBKiirotori.Data.MessageObject
import           LBKiirotori.Database.Redis       (AccessToken (..))
import           LBKiirotori.Internal.HTTP        (reqGet)
import           LBKiirotori.Internal.Utils       (decodeJSON,
                                                   stripFirstToLowerLabeledOption)

data ProfileMyselfResp = ProfileMyselfResp {
    pmUserId         :: T.Text
  , pmBasicId        :: T.Text
  , pmPremiumId      :: Maybe T.Text
  , pmDisplayName    :: T.Text
  , pmPictureUrl     :: Maybe T.Text
  , pmChatMode       :: T.Text
  , pmMarkAsReadMode :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON ProfileMyselfResp where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 2

instance FromJSON ProfileMyselfResp where
    parseJSON (Object v) = ProfileMyselfResp
        <$> v .: "userId"
        <*> v .: "basicId"
        <*> v .:? "premiumId"
        <*> v .: "displayName"
        <*> v .:? "pictureUrl"
        <*> v .: "chatMode"
        <*> v .: "markAsReadMode"
    parseJSON invalid = prependFailure "parsing ProfileMyselfResp failed, "
        $ typeMismatch "Object" invalid

reqGetMyself :: B.ByteString -> Request
reqGetMyself = reqGet "https://api.line.me/v2/bot/info"

profileMyself :: (MonadThrow m, MonadIO m) => AccessToken -> m ProfileMyselfResp
profileMyself token = do
    (statusCode, body) <- (getResponseStatusCode &&& getResponseBody)
        <$> httpLbs (reqGetMyself (atToken token))
    if statusCode == 200 then decodeJSON body
    else throwString ||| throw
        $ (eitherDecode body :: Either String MessageErrorResp)
