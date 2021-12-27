module LBKiirotori.Webhook.EventObject.LineBotHandler.Core where

import qualified Data.ByteString                                     as B
import qualified Data.Text                                           as T
import           Database.Redis                                      (Connection)
import           LBKiirotori.AccessToken.Config                      (AccessToken (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler.Data (LineBotHandler)

askLineKId :: LineBotHandler B.ByteString

askLineChanId :: LineBotHandler B.ByteString

askLineChanSecret :: LineBotHandler B.ByteString

askLineChanName :: LineBotHandler T.Text

askLineUserId :: LineBotHandler B.ByteString

askLineJWKSet :: LineBotHandler B.ByteString

askRedisConn :: LineBotHandler Connection
