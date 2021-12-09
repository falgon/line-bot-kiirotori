module LBKiirotori.Webhook.EventObject.LineBotHandler.Core where

import LBKiirotori.AccessToken.Config (AccessToken (..))
import LBKiirotori.Webhook.EventObject.LineBotHandler.Data (LineBotHandler)
import qualified Data.ByteString as B
import Database.Redis (Connection)

askLineKId :: LineBotHandler B.ByteString

askLineChanId :: LineBotHandler B.ByteString

askLineChanSecret :: LineBotHandler B.ByteString

askLineUserId :: LineBotHandler B.ByteString

askLineJWKSet :: LineBotHandler B.ByteString

askRedisConn :: LineBotHandler Connection
