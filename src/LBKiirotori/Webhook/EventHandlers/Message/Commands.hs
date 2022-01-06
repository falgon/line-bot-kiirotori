module LBKiirotori.Webhook.EventHandlers.Message.Commands (
    cmd
) where

import           Text.Megaparsec                                         as M

import           LBKiirotori.Webhook.EventHandlers.Message.Commands.Auth
import           LBKiirotori.Webhook.EventHandlers.Message.Commands.Echo
import           LBKiirotori.Webhook.EventHandlers.Message.Commands.Help
import           LBKiirotori.Webhook.EventHandlers.Message.Event         (MessageEvent)

-- <cmd> ::= <echo>
--      | <auth>
--      | <help>
cmd :: MessageEvent ()
cmd = M.choice [
    M.try echoCmd
  , M.try authCmd
  , M.try helpCmd
  , pure ()
  ]
