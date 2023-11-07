{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module LBKiirotori.Webhook.EventHandlers.LINE.Message (
    messageEvent
) where

import           Control.Exception.Safe                                   (throwString)
import           Control.Monad.Logger                                     (logError,
                                                                           logInfo)

import           LBKiirotori.Internal.Utils                               (tshow)
import           LBKiirotori.Webhook.EventHandlers.LINE.Message.Commands  (cmd)
import           LBKiirotori.Webhook.EventHandlers.LINE.Message.Event
import           LBKiirotori.Webhook.EventHandlers.LINE.Message.MentionMe
import           LBKiirotori.Webhook.EventObject.Core                     (LineEventObject (..),
                                                                           LineEventType (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler           (LineBotHandler)

messageEvent :: LineEventObject
    -> LineBotHandler ()
messageEvent e
    | lineEventType e == LineEventTypeMessage =
        case (,) <$> lineEventReplyToken e <*> lineEventMessage e of
            Nothing -> $(logError) "expected reply token and message object"
            Just (tk, mobj) -> repliedMe mobj
                >>= maybe
                    ($(logInfo) "the message is not replied me")
                    (runMessageEvent cmd (MessageEventData (Just tk) e))
    | otherwise = throwString $ mconcat [ "expected message event: ", show e ]
