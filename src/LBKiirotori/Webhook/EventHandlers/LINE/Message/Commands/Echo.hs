{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module LBKiirotori.Webhook.EventHandlers.LINE.Message.Commands.Echo (
    echoCmd
) where

import           Control.Monad.Logger                                  (logInfo)
import           Control.Monad.Trans                                   (lift)
import qualified Text.Megaparsec                                       as M
import qualified Text.Megaparsec.Char                                  as MC

import           LBKiirotori.Webhook.EventHandlers.LINE.Message.Event  (MessageEvent)
import           LBKiirotori.Webhook.EventHandlers.LINE.Message.Parser (lexeme)
import           LBKiirotori.Webhook.EventHandlers.LINE.Message.Utils  (replyOneText)

-- | the echo command, echo the sent message as it is
-- <echo> ::= "echo" <space> (<space>*) <string>
echoCmd :: MessageEvent ()
echoCmd = do
    txtBody <- MC.string' "echo" *> lexeme MC.space1 *> M.getInput
    lift (lift ($(logInfo) "requested echo command, send reply message"))
        >> replyOneText txtBody
