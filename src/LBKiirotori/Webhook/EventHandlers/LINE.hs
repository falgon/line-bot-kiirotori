{-# LANGUAGE TemplateHaskell #-}
module LBKiirotori.Webhook.EventHandlers.LINE where

import           Control.Monad.Logger                           (logError)

import           LBKiirotori.Internal.Utils                     (tshow)
import           LBKiirotori.Webhook.EventHandlers.Class
import           LBKiirotori.Webhook.EventHandlers.LINE.Join
import           LBKiirotori.Webhook.EventHandlers.LINE.Message
import           LBKiirotori.Webhook.EventObject                (LineEventObject (..),
                                                                 LineEventType (..))

instance EventHandler LineEventObject where
    handle x
        | lineEventType x == LineEventTypeJoin = joinEvent x
        | lineEventType x == LineEventTypeMessage = messageEvent x
        | otherwise = $(logError) (tshow x)
