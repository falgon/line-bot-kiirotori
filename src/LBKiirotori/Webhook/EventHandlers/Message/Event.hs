{-# LANGUAGE TemplateHaskell #-}
module LBKiirotori.Webhook.EventHandlers.Message.Event (
    MessageEventData (..)
  , MessageEvent
  , runMessageEvent
) where

import           Control.Arrow                                  ((|||))
import           Control.Monad.Logger                           (logError)
import           Control.Monad.Trans.State                      (StateT,
                                                                 evalStateT)
import qualified Data.Text                                      as T
import           Data.Void                                      (Void)
import qualified Text.Megaparsec                                as M

import           LBKiirotori.Webhook.EventObject.Core           (LineEventObject (..))
import           LBKiirotori.Webhook.EventObject.EventMessage   (LineEventMessage (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler

data MessageEventData = MessageEventData {
    medTk  :: Maybe T.Text
  , medLEO :: LineEventObject
  }

type MessageEvent = M.ParsecT Void T.Text (StateT MessageEventData LineBotHandler)

runMessageEvent :: MessageEvent ()
    -> MessageEventData
    -> T.Text
    -> LineBotHandler ()
runMessageEvent cmd evData txtBody = evalStateT (M.runParserT cmd mempty txtBody) evData
    >>= (($(logError) . T.pack . M.errorBundlePretty) ||| pure)
