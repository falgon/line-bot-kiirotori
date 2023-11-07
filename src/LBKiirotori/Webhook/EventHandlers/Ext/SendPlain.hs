{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell #-}
module LBKiirotori.Webhook.EventHandlers.Ext.SendPlain (
    sendPlain
) where

import           Control.Monad.Logger                           (logError,
                                                                 logInfo)
import           Control.Monad.Parallel                         (bindM2)
import           Control.Monad.Trans.Maybe                      (MaybeT (..))

import           LBKiirotori.AccessToken                        (getAccessToken)
import           LBKiirotori.API.PushMessage
import           LBKiirotori.Config                             (LBKiirotoriAppConfig (..),
                                                                 LBKiirotoriConfig (..))
import           LBKiirotori.Data.MessageObject                 (MessageBody (..),
                                                                 textMessage)
import           LBKiirotori.Internal.Utils                     (hoistMaybe,
                                                                 tshow)
import           LBKiirotori.Webhook.EventObject.Core           (ExtEventObject (..),
                                                                 ExtEventType (..),
                                                                 LineEventObject)
import           LBKiirotori.Webhook.EventObject.LineBotHandler


sendPlain :: ExtEventObject
    -> LineBotHandler ()
sendPlain x
    | extEventType x == ExtEventTypeSendPlain = $(logInfo) ("sendPlain instruction is comming from " <> extEventSource x)
        >> runMaybeT ((,) <$> hoistMaybe (extEventTarget x) <*> hoistMaybe (extEventMessages x))
        >>= maybe unexpectedUndefined (bindM2 pushMessage getAccessToken . uncurry messageObject)
    | otherwise = $(logError) "expected sendPlain event"
        >> $(logError) (tshow x)
    where
        unexpectedUndefined = $(logError)
                ("sendPlain requested but target or message is not defined: " <> tshow x)
        messageObject target messages = pure $ PushMessage {
            pmTo = target
          , pmMessages = map (\x -> MBText $ textMessage x Nothing Nothing) messages
          }
