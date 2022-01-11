{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.Webhook.EventObject.EventType (
    LineEventType (..)
) where

import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             genericToJSON)
import           Data.Aeson.Types           (Value (..), prependFailure,
                                             typeMismatch)
import           GHC.Generics

import           LBKiirotori.Internal.Utils (stripFirstToLowerLabeledOption)

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#webhook-event-objects
data LineEventType = LineEventTypeMessage
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
    deriving (Eq, Show, Generic)

instance FromJSON LineEventType where
    parseJSON (String "message")            = pure LineEventTypeMessage
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
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 13

