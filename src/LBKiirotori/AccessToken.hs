module LBKiirotori.AccessToken (
    getAccessToken
  , getValidAccessTokenKIds
  , revokeCurrentAccessToken
  , newConn
) where

import           Control.Monad.IO.Class                         (MonadIO (..))
import           Control.Monad.Parallel                         (bindM2)
import           Data.String                                    (IsString (..))
import qualified Data.Text                                      as T
import           Data.Time.Clock                                (getCurrentTime)
import           Database.Redis                                 (Connection)

import           LBKiirotori.AccessToken.Core
import           LBKiirotori.AccessToken.Redis
import           LBKiirotori.Internal.Utils
import           LBKiirotori.Webhook.EventObject.LineBotHandler (LineBotHandler,
                                                                 askLineChanId,
                                                                 askLineChanSecret)

getAccessToken :: LineBotHandler AccessToken
getAccessToken = takeValidToken
    >>= maybe (bindM2 writeToken (liftIO getCurrentTime) reqAccessToken) pure

getValidAccessTokenKIds :: LineBotHandler [T.Text]
getValidAccessTokenKIds = verifiedKIds <$> reqAllValidCATKIds

revokeCurrentAccessToken :: LineBotHandler ()
revokeCurrentAccessToken = takeToken
    >>= maybe (pure ()) reqRevokeChannelAccess
