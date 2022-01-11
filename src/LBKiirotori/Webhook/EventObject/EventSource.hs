{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.Webhook.EventObject.EventSource (
    LineEventSourceType (..)
  , LineEventSource (..)
) where

import           Data.Aeson       (FromJSON (..), ToJSON (..), object, (.:),
                                   (.:?), (.=))
import           Data.Aeson.Types (Value (..), prependFailure, typeMismatch)
import qualified Data.Text        as T

data LineEventSourceType = LineEventSourceTypeUser
    | LineEventSourceTypeGroup
    | LineEventSourceTypeRoom
    deriving (Eq, Ord, Show, Enum)

instance FromJSON LineEventSourceType where
    parseJSON (String "user")   = pure LineEventSourceTypeUser
    parseJSON (String "group")  = pure LineEventSourceTypeGroup
    parseJSON (String "room")   = pure LineEventSourceTypeRoom
    parseJSON invalid           = prependFailure "parsing LineEventMode failed, "
        $ typeMismatch "String" invalid

instance ToJSON LineEventSourceType where
    toJSON LineEventSourceTypeUser  = String "user"
    toJSON LineEventSourceTypeGroup = String "group"
    toJSON LineEventSourceTypeRoom  = String "room"

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#webhook-event-objects
data LineEventSource = LineEventSource {
    lineEventSrcType    :: LineEventSourceType
  , lineEventSrcUserId  :: Maybe T.Text
  , lineEventSrcGroupId :: Maybe T.Text
  , lineEventSrcRoomId  :: Maybe T.Text
  } deriving (Eq, Show)

instance FromJSON LineEventSource where
    parseJSON (Object v) = LineEventSource
        <$> v .: "type"
        <*> v .:? "userId"
        <*> v .:? "groupId"
        <*> v .:? "roomId"
    parseJSON invalid = prependFailure "parsing LineEventSource failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventSource where
    toJSON (LineEventSource LineEventSourceTypeUser userId _ _) = object [
        "type" .= toJSON LineEventSourceTypeUser
      , "userId" .= userId
      ]
    toJSON (LineEventSource LineEventSourceTypeGroup userId groupId _) = object [
        "type" .= toJSON LineEventSourceTypeGroup
      , "userId" .= userId
      , "groupId" .= groupId
      ]
    toJSON (LineEventSource LineEventSourceTypeRoom userId _ roomId) = object [
        "type" .= toJSON LineEventSourceTypeRoom
      , "userId" .= userId
      , "roomId" .= roomId
      ]

