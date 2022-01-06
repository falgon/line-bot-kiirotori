{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.API.Profile.FriendUser (
    profileFriendUser
  , ProfileFriendUserResp (..)
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
import           LBKiirotori.Internal.Utils       (decodeJSON, stripFirstToLowerLabeledOption)

data ProfileFriendUserResp = ProfileFriendUserResp {
    pfuDisplayName   :: T.Text
  , pfuUserId        :: T.Text
  , pfuLanguage      :: T.Text
  , pfuPictureUrl    :: Maybe T.Text
  , pfuStatusMessage :: Maybe T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON ProfileFriendUserResp where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 3

instance FromJSON ProfileFriendUserResp where
    parseJSON (Object v) = ProfileFriendUserResp
        <$> v .: "displayName"
        <*> v .: "userId"
        <*> v .: "language"
        <*> v .:? "pictureUrl"
        <*> v .:? "statusMessage"

reqGetFriendUser :: String -> B.ByteString -> Request
reqGetFriendUser userId = reqGet ("https://api.line.me/v2/bot/profile/" <> userId)

profileFriendUser :: (MonadThrow m, MonadIO m) => AccessToken -> String -> m ProfileFriendUserResp
profileFriendUser token userId = do
    (statusCode, body) <- (getResponseStatusCode &&& getResponseBody)
        <$> httpLbs (reqGetFriendUser userId (atToken token))
    if statusCode == 200 then decodeJSON body
    else throwString ||| throw
        $ (eitherDecode body :: Either String MessageErrorResp)
