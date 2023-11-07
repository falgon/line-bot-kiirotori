{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module LBKiirotori.Webhook.EventHandlers.Ext where

import           Control.Monad.Logger                            (logError)

import           LBKiirotori.Internal.Utils                      (tshow)
import           LBKiirotori.Webhook.EventHandlers.Class
import           LBKiirotori.Webhook.EventHandlers.Ext.SendPlain (sendPlain)
import           LBKiirotori.Webhook.EventObject                 (ExtEventObject (..),
                                                                  ExtEventType (..))

instance EventHandler ExtEventObject where
    handle x
        | extEventType x == ExtEventTypeSendPlain = sendPlain x
        | otherwise = $(logError) ("invalid requests: " <> tshow x)
