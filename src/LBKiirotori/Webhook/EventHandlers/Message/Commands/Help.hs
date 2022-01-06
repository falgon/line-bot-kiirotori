{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module LBKiirotori.Webhook.EventHandlers.Message.Commands.Help (
    helpCmd
) where

import           Control.Monad.Logger                             (logInfo)
import           Control.Monad.Trans                              (lift)
import qualified Text.Megaparsec                                  as M
import qualified Text.Megaparsec.Char                             as MC

import           LBKiirotori.Webhook.EventHandlers.Message.Event  (MessageEvent)
import           LBKiirotori.Webhook.EventHandlers.Message.Parser (lexeme)
import           LBKiirotori.Webhook.EventHandlers.Message.Utils  (replyOneText)

helpCmd :: MessageEvent ()
helpCmd = (lexeme (MC.string' "help") *> M.eof)
    *> lift (lift $ $(logInfo) "requested help command, send reply message")
    *> replyOneText "help!"

