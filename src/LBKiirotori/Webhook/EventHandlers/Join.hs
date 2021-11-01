{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module LBKiirotori.Webhook.EventHandlers.Join (
    joinEvent
) where

import           Control.Monad.Logger                           (logError)
import           Control.Monad.Reader                           (asks)

import           LBKiirotori.API.ReplyMessage
import           LBKiirotori.Data.MessageObject                 (MessageBody (..),
                                                                 textMessage)
import           LBKiirotori.Internal.Utils                     (tshow)
import           LBKiirotori.Webhook.EventObject.Core           (LineEventObject (..),
                                                                 LineEventType (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler

joinEvent :: LineEventObject
    -> LineBotHandler ()
joinEvent e
    | lineEventType e == LineEventTypeJoin = case lineEventReplyToken e of
        Nothing -> $(logError) "expected reply token"
        Just tk -> do
            caToken <- asks lbhChannelSecret
            replyMessage caToken $ ReplyMessage {
                replyMessageReplyToken = tk
              , replyMessageMessages = [
                    MBText $ textMessage "hello" Nothing Nothing
                  ]
              , replyMessageNotificationDisabled = Nothing
              }
    | otherwise = $(logError) "expected join event"
        >> $(logError) (tshow e)
