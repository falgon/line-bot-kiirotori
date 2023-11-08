{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module LBKiirotori.Webhook.EventHandlers.LINE.Join (
    joinEvent
) where

import           Control.Monad.Logger                           (logError,
                                                                 logInfo)
import           Control.Monad.Reader                           (asks)

import           LBKiirotori.AccessToken                        (getAccessToken)
import           LBKiirotori.API.ReplyMessage
import           LBKiirotori.Config                             (LBKiirotoriAppConfig (..),
                                                                 LBKiirotoriConfig (..))
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
            caToken <- getAccessToken
            joinMessage <- asks $ cfgAppWelcome . cfgApp . lbhCfg
            $(logInfo) "send reply message"
            replyMessage caToken $ ReplyMessage {
                replyMessageReplyToken = tk
              , replyMessageMessages = [
                    MBText $ textMessage joinMessage Nothing Nothing
                  ]
              , replyMessageNotificationDisabled = Nothing
              }
    | otherwise = $(logError) "expected join event"
        >> $(logError) (tshow e)
