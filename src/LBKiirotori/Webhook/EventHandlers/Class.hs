module LBKiirotori.Webhook.EventHandlers.Class (
    EventHandler (..)
) where

import           LBKiirotori.Webhook.EventObject.LineBotHandler.Data (LineBotHandler)

class EventHandler a where
    handle :: a -> LineBotHandler ()
