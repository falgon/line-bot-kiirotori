{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.API.Count.Room (
    countRoom
  , CountRoomResp (..)
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
import           Network.HTTP.Conduit             (RequestBody (..))
import           Network.HTTP.Simple

import           LBKiirotori.API.MessageErrorResp
import           LBKiirotori.Data.MessageObject
import           LBKiirotori.Database.Redis       (AccessToken (..))
import           LBKiirotori.Internal.HTTP        (reqGet)
import           LBKiirotori.Internal.Utils       (decodeJSON, stripFirstToLowerLabeledOption)

data CountRoomResp = CountRoomResp {
    cmCount :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON CountRoomResp where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 2

instance FromJSON CountRoomResp where
    parseJSON (Object v) = CountRoomResp
        <$> v .: "count"

reqCountRoom :: String
        -> B.ByteString
        -> Request
reqCountRoom roomId = reqGet
    $ mconcat [
        "https://api.line.me/v2/bot/room/"
      , roomId
      , "/members/count"
      ]

countRoom :: (MonadThrow m, MonadIO m)
        => AccessToken
        -> String
        -> m CountRoomResp
countRoom token roomId = do
    (statusCode, body) <- (getResponseStatusCode &&& getResponseBody)
        <$> httpLbs (reqCountRoom roomId (atToken token))
    if statusCode == 200 then decodeJSON body
    else throwString ||| throw
        $ (eitherDecode body :: Either String MessageErrorResp)
