{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module LBKiirotori.Webhook.EventHandlers.LINE.Message.Commands.Help (
    helpCmd
) where

import           Control.Monad.Logger                                  (logInfo)
import           Control.Monad.Trans                                   (lift)
import qualified Text.Megaparsec                                       as M
import qualified Text.Megaparsec.Char                                  as MC

import           LBKiirotori.Webhook.EventHandlers.LINE.Message.Event  (MessageEvent)
import           LBKiirotori.Webhook.EventHandlers.LINE.Message.Parser (lexeme)
import           LBKiirotori.Webhook.EventHandlers.LINE.Message.Utils  (replyOneText)

helpCmd :: MessageEvent ()
helpCmd = (lexeme (MC.string' "help") *> M.eof)
    *> lift (lift $ $(logInfo) "requested help command, send reply message")
    *> replyOneText "help!"
