{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module LBKiirotori.Webhook.EventHandlers.Ext.SendPlain (
    sendPlain
) where

import           Control.Monad.Logger                           (logError,
                                                                 logInfo)
import           Control.Monad.Parallel                         (bindM2)

import           LBKiirotori.AccessToken                        (getAccessToken)
import           LBKiirotori.API.PushMessage
import           LBKiirotori.Config                             (LBKiirotoriAppConfig (..),
                                                                 LBKiirotoriConfig (..))
import           LBKiirotori.Data.MessageObject                 (MessageBody (..),
                                                                 textMessage)
import           LBKiirotori.Internal.Utils                     (tshow)
import           LBKiirotori.Webhook.EventObject.Core           (ExtEventObject (..),
                                                                 ExtEventType (..),
                                                                 LineEventObject)
import           LBKiirotori.Webhook.EventObject.LineBotHandler

sendPlain :: ExtEventObject
    -> LineBotHandler ()
sendPlain x
    | extEventType x == ExtEventTypeSendPlain = do
        $(logInfo) ("sendPlain instruction is comming from " <> extEventSource x)
        case (extEventTarget x, extEventMessages x) of
            (Just target, Just messages) ->
                bindM2 pushMessage getAccessToken $ pure $ messageObject target messages
            _ -> unexpectedUndefined
    | otherwise = $(logError) "expected join event"
        >> $(logError) (tshow x)
    where
        unexpectedUndefined = $(logError)
                ("sendPlain requested but target or message is not defined: " <> tshow x)
        messageObject target messages = PushMessage {
            pmTo = target
          , pmMessages = map (\x -> MBText $ textMessage x Nothing Nothing) messages
          }
