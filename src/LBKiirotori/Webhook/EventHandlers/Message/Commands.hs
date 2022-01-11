{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.Webhook.EventHandlers.Message.Commands (
    cmd
) where

import           Control.Monad.Extra                                     (ifM)
import           Control.Monad.Trans                                     (lift)
import           Control.Monad.Trans.Reader                              (asks)
import           Data.Maybe                                              (isJust)
import           Text.Megaparsec                                         as M

import           LBKiirotori.Config                                      (LBKiirotoriAppConfig (..),
                                                                          LBKiirotoriConfig (..))
import           LBKiirotori.Webhook.EventHandlers.Message.Commands.Auth
import           LBKiirotori.Webhook.EventHandlers.Message.Commands.Echo
import           LBKiirotori.Webhook.EventHandlers.Message.Commands.Help
import           LBKiirotori.Webhook.EventHandlers.Message.Event         (MessageEvent)
import           LBKiirotori.Webhook.EventHandlers.Message.Utils         (replyOneText)
import           LBKiirotori.Webhook.EventObject.LineBotHandler.Data     (LineBotHandlerConfig (..))

-- <authed cmd> ::= <echo>
--      | <auth>
--      | <help>
authedCmd :: MessageEvent ()
authedCmd = M.choice [
    M.try echoCmd
  , M.try authCmd
  , M.try helpCmd
  , lift (lift $ asks $ cfgAppUnknown . cfgApp . lbhCfg)
        >>= replyOneText
  ]

-- <not authed cmd> ::= <auth>
--      | <auth help message>
notAuthedCmd :: MessageEvent ()
notAuthedCmd = M.choice [
    M.try authCmd
  , lift (lift $ asks $ cfgAppWelcome . cfgApp . lbhCfg)
        >>= replyOneText
  ]

-- <cmd> ::= <authed cmd>
--      | <not authed cmd>
cmd :: MessageEvent ()
cmd = ifM (isJust <$> checkAuthed)
    authedCmd
    notAuthedCmd
