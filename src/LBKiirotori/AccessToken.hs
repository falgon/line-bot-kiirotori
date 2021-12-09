module LBKiirotori.AccessToken (
    getAccessToken
  , getValidAccessTokenKIds
  , revokeCurrentAccessToken
  , newConn
) where

import           Control.Monad                                  (join, liftM2)
import           Control.Monad.IO.Class                         (MonadIO (..))
import           Data.String                                    (IsString (..))
import qualified Data.Text                                      as T
import           Data.Time.Clock                                (getCurrentTime)
import           Database.Redis                                 (Connection)

import           LBKiirotori.AccessToken.Core
import           LBKiirotori.AccessToken.Redis
import           LBKiirotori.Webhook.EventObject.LineBotHandler (LineBotHandler,
                                                                 askLineChanId,
                                                                 askLineChanSecret)

getAccessToken :: LineBotHandler AccessToken
getAccessToken = takeValidToken
    >>= maybe (join $ liftM2 writeToken (liftIO getCurrentTime) reqAccessToken) pure

getValidAccessTokenKIds :: LineBotHandler [T.Text]
getValidAccessTokenKIds = verifiedKIds <$> reqAllValidCATKIds

revokeCurrentAccessToken :: LineBotHandler ()
revokeCurrentAccessToken = takeToken
    >>= maybe (pure ()) reqRevokeChannelAccess
