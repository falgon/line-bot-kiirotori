{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.Webhook.EventObject.EventType (
    LineEventType (..)
) where

import           Data.Aeson       (FromJSON (..), ToJSON (..))
import           Data.Aeson.Types (Value (..), prependFailure, typeMismatch)

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#webhook-event-objects
data LineEventType = LineEventTypeMessage
    | LineEventTypeText
    | LineEventTypeImage
    | LineEventTypeVideo
    | LineEventTypeAudio
    | LineEventTypeFile
    | LineEventTypeLocation
    | LineEventTypeSticker
    | LineEventTypeUnsend
    | LineEventTypeFollow
    | LineEventTypeUnfollow
    | LineEventTypeJoin
    | LineEventTypeLeave
    | LineEventTypeMemberJoined
    | LineEventTypeMemberLeft
    | LineEventTypePostback
    | LineEventTypeVideoPlayComplete
    | LineEventTypeBeacon
    | LineEventTypeAccountLink
    | LineEventTypeThings
    deriving (Eq, Show)

instance FromJSON LineEventType where
    parseJSON (String "message")            = pure LineEventTypeMessage
    parseJSON (String "text")               = pure LineEventTypeText
    parseJSON (String "image")              = pure LineEventTypeImage
    parseJSON (String "video")              = pure LineEventTypeVideo
    parseJSON (String "audio")              = pure LineEventTypeAudio
    parseJSON (String "file")               = pure LineEventTypeFile
    parseJSON (String "location")           = pure LineEventTypeLocation
    parseJSON (String "sticker")            = pure LineEventTypeSticker
    parseJSON (String "unsend")             = pure LineEventTypeUnsend
    parseJSON (String "follow")             = pure LineEventTypeFollow
    parseJSON (String "unfollow")           = pure LineEventTypeUnfollow
    parseJSON (String "join")               = pure LineEventTypeJoin
    parseJSON (String "leave")              = pure LineEventTypeLeave
    parseJSON (String "memberJoined")       = pure LineEventTypeMemberJoined
    parseJSON (String "memberLeft")         = pure LineEventTypeMemberLeft
    parseJSON (String "postback")           = pure LineEventTypePostback
    parseJSON (String "videoPlayComplete")  = pure LineEventTypeVideoPlayComplete
    parseJSON (String "beacon")             = pure LineEventTypeBeacon
    parseJSON (String "accountLink")        = pure LineEventTypeAccountLink
    parseJSON (String "things")             = pure LineEventTypeThings
    parseJSON invalid                       = prependFailure "parsing LineEventType failed, "
        $ typeMismatch "String" invalid

instance ToJSON LineEventType where
    toJSON LineEventTypeMessage           = String "message"
    toJSON LineEventTypeText              = String "text"
    toJSON LineEventTypeImage             = String "image"
    toJSON LineEventTypeVideo             = String "video"
    toJSON LineEventTypeAudio             = String "audio"
    toJSON LineEventTypeFile              = String "file"
    toJSON LineEventTypeLocation          = String "location"
    toJSON LineEventTypeSticker           = String "sticker"
    toJSON LineEventTypeUnsend            = String "unsend"
    toJSON LineEventTypeFollow            = String "follow"
    toJSON LineEventTypeUnfollow          = String "unfollow"
    toJSON LineEventTypeJoin              = String "join"
    toJSON LineEventTypeLeave             = String "leave"
    toJSON LineEventTypeMemberJoined      = String "memberJoined"
    toJSON LineEventTypeMemberLeft        = String "memberLeft"
    toJSON LineEventTypePostback          = String "postback"
    toJSON LineEventTypeVideoPlayComplete = String "videoPlayComplete"
    toJSON LineEventTypeBeacon            = String "beacon"
    toJSON LineEventTypeAccountLink       = String "accountLink"
    toJSON LineEventTypeThings            = String "things"
