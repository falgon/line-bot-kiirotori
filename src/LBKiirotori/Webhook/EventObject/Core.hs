{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module LBKiirotori.Webhook.EventObject.Core (
    LineEventType (..)
  , LineEventObject (..)
) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict                             as HM
import           Data.Maybe                                      (catMaybes)
import           Data.Scientific
import qualified Data.Text                                       as T
import           Data.Word                                       (Word32)

import           LBKiirotori.Internal.Utils                      (tshow)
import           LBKiirotori.Webhook.EventObject.DeliveryContext
import           LBKiirotori.Webhook.EventObject.EventMessage
import           LBKiirotori.Webhook.EventObject.EventMode
import           LBKiirotori.Webhook.EventObject.EventSource
import           LBKiirotori.Webhook.EventObject.EventType

data LineEventObject = LineEventObject {
    lineEventType            :: LineEventType
  , lineEventMode            :: LineEventMode
  , lineEventTimestamp       :: Scientific
  , lineEventSource          :: Maybe LineEventSource
  , lineEventWebhookEventId  :: T.Text
  , lineEventDeliveryContext :: LineEventDeliveryContext
  , lineEventReplyToken      :: Maybe T.Text
  , lineEventMessage         :: Maybe LineEventMessage
  } deriving Show

instance FromJSON LineEventObject where
    parseJSON (Object v) = LineEventObject
        <$> v .: "type"
        <*> v .: "mode"
        <*> v .: "timestamp"
        <*> v .:? "source"
        <*> v .: "webhookEventId"
        <*> v .: "deliveryContext"
        <*> v .:? "replyToken"
        <*> v .:? "message"
    parseJSON invalid = prependFailure "parsing LineEventObject failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventObject where
    toJSON v = Object $ HM.fromList $ catMaybes [
        Just ("type", toJSON $ lineEventType v)
      , Just ("mode", toJSON $ lineEventMode v)
      , Just ("timestamp", Number $ lineEventTimestamp v)
      , ("source",) . toJSON <$> lineEventSource v
      , Just ("webhookEventId", String $ lineEventWebhookEventId v)
      , Just ("deliveryContext", toJSON $ lineEventDeliveryContext v)
      , ("replyToken",) . String <$> lineEventReplyToken v
      , ("message",) . toJSON <$> lineEventMessage v
      ]
