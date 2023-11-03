{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.Webhook.EventObject.DeliveryContext (
    LineEventDeliveryContext (..)
  , ExtDeliveryContext (..)
) where

import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             genericToJSON, (.:))
import           Data.Aeson.Types           (Value (..), prependFailure,
                                             typeMismatch)
import qualified Data.Text                  as T
import           GHC.Generics

import           LBKiirotori.Internal.Utils (stripFirstToLowerLabeledOption)

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#webhook-event-objects
data LineEventDeliveryContext = LineEventDeliveryContext {
    lineEventDCIsRedelivery :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventDeliveryContext where
    parseJSON (Object v) = LineEventDeliveryContext
        <$> v .: "isRedelivery"
    parseJSON invalid = prependFailure "parsing LineEventSource failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventDeliveryContext where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 11


data ExtDeliveryContext = ExtDeliveryContext {
    extDCIsRedelivery :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON ExtDeliveryContext where
    parseJSON (Object v) = ExtDeliveryContext
        <$> v .: "isRedelivery"
    parseJSON invalid = prependFailure "parsing ExtDeliveryContext failed, "
        $ typeMismatch "Object" invalid

instance ToJSON ExtDeliveryContext where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 5
