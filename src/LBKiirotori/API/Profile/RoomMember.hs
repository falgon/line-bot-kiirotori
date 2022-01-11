{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.API.Profile.RoomMember (
    profileRoomMember
  , ProfileRoomMemberResp (..)
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

data ProfileRoomMemberResp = ProfileRoomMemberResp {
    prmDisplayName :: T.Text
  , prmUserId      :: T.Text
  , prmPictureUrl  :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON ProfileRoomMemberResp where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 3

instance FromJSON ProfileRoomMemberResp where
    parseJSON (Object v) = ProfileRoomMemberResp
        <$> v .: "displayName"
        <*> v .: "userId"
        <*> v .: "pictureUrl"

reqProfileRoomMember :: String
        -> String
        -> B.ByteString
        -> Request
reqProfileRoomMember userId roomId = reqGet
    $ mconcat [
        "https://api.line.me/v2/bot/room/"
      , roomId
      , "/member/"
      , userId
      ]

profileRoomMember :: (MonadThrow m, MonadIO m)
        => AccessToken
        -> String
        -> String
        -> m ProfileRoomMemberResp
profileRoomMember token userId roomId = do
    (statusCode, body) <- (getResponseStatusCode &&& getResponseBody)
        <$> httpLbs (reqProfileRoomMember userId roomId (atToken token))
    if statusCode == 200 then decodeJSON body
    else throwString ||| throw
        $ (eitherDecode body :: Either String MessageErrorResp)
