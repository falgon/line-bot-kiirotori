{-# LANGUAGE TemplateHaskell #-}
module LBKiirotori.Webhook.EventHandlers.Message.Event (
    MessageEventData (..)
  , MessageEvent
  , runMessageEvent
  , getLineEventSrc
) where

import           Control.Arrow                                  ((|||))
import           Control.Exception.Safe                         (throwString)
import           Control.Monad.Logger                           (logError)
import           Control.Monad.Trans                            (MonadTrans (..))
import           Control.Monad.Trans.State                      (StateT,
                                                                 evalStateT,
                                                                 gets)
import qualified Data.Text                                      as T
import           Data.Void                                      (Void)
import qualified Text.Megaparsec                                as M

import           LBKiirotori.Internal.Utils                     (fromMaybeM)
import           LBKiirotori.Webhook.EventObject.Core           (LineEventObject (..))
import           LBKiirotori.Webhook.EventObject.EventMessage   (LineEventMessage (..))
import           LBKiirotori.Webhook.EventObject.EventSource    (LineEventSource (..))
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

getLineEventSrc :: MessageEvent LineEventSource
getLineEventSrc = lift (gets $ lineEventSource . medLEO)
    >>= fromMaybeM (lift $ lift $ throwString "Unexpected behaviour: the event source unknown")
