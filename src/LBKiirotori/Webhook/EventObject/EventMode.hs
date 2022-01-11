{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.Webhook.EventObject.EventMode (
    LineEventMode (..)
) where

import           Data.Aeson       (FromJSON (..), ToJSON (..))
import           Data.Aeson.Types (Value (..), prependFailure, typeMismatch)

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#webhook-event-objects
data LineEventMode = LineEventModeActive
    | LineEventModeStandby
    deriving (Eq, Show)

instance FromJSON LineEventMode where
    parseJSON (String "active")     = pure LineEventModeActive
    parseJSON (String "standby")    = pure LineEventModeStandby
    parseJSON invalid               = prependFailure "parsing LineEventMode failed, "
        $ typeMismatch "String" invalid

instance ToJSON LineEventMode where
    toJSON LineEventModeActive  = String "active"
    toJSON LineEventModeStandby = String "standby"
