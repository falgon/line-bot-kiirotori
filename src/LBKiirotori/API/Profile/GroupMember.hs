{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.API.Profile.GroupMember (
    profileGroupMember
  , ProfileGroupMemberResp (..)
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

data ProfileGroupMemberResp = ProfileGroupMemberResp {
    pgmDisplayName :: T.Text
  , pgmUserId      :: T.Text
  , pgmPictureUrl  :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON ProfileGroupMemberResp where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 3

instance FromJSON ProfileGroupMemberResp where
    parseJSON (Object v) = ProfileGroupMemberResp
        <$> v .: "displayName"
        <*> v .: "userId"
        <*> v .: "pictureUrl"

reqProfileGroupMember :: String
        -> String
        -> B.ByteString
        -> Request
reqProfileGroupMember userId groupId = reqGet
    $ mconcat [
        "https://api.line.me/v2/bot/group/"
      , groupId
      , "/member/"
      , userId
      ]

profileGroupMember :: (MonadThrow m, MonadIO m)
        => AccessToken
        -> String
        -> String
        -> m ProfileGroupMemberResp
profileGroupMember token userId groupId = do
    (statusCode, body) <- (getResponseStatusCode &&& getResponseBody)
        <$> httpLbs (reqProfileGroupMember userId groupId (atToken token))
    if statusCode == 200 then decodeJSON body
    else throwString ||| throw
        $ (eitherDecode body :: Either String MessageErrorResp)
