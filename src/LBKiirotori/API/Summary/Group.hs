{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.API.Summary.Group (
    groupSummary
  , GroupSummaryResp (..)
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

data GroupSummaryResp = GroupSummaryResp {
    gsGroupId    :: T.Text
  , gsGroupName  :: T.Text
  , gsPictureUrl :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON GroupSummaryResp where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 2

instance FromJSON GroupSummaryResp where
    parseJSON (Object v) = GroupSummaryResp
        <$> v .: "groupId"
        <*> v .: "groupName"
        <*> v .: "pictureUrl"

reqGroupSummary :: String
        -> B.ByteString
        -> Request
reqGroupSummary groupId = reqGet
    $ mconcat [
        "https://api.line.me/v2/bot/group/"
      , groupId
      , "/summary"
      ]

groupSummary :: (MonadThrow m, MonadIO m)
        => AccessToken
        -> String
        -> m GroupSummaryResp
groupSummary token groupId = do
    (statusCode, body) <- (getResponseStatusCode &&& getResponseBody)
        <$> httpLbs (reqGroupSummary groupId (atToken token))
    if statusCode == 200 then decodeJSON body
    else throwString ||| throw
        $ (eitherDecode body :: Either String MessageErrorResp)
