module LBKiirotori.AccessToken.Class (
    AccessTokenMonad (..)
) where

import qualified Data.ByteString as BS
import qualified Database.Redis  as R

class Monad m => AccessTokenMonad m where
    lineJWKSet :: m BS.ByteString
    lineKId :: m BS.ByteString
    lineChanId :: m BS.ByteString
    lineChanSecret :: m BS.ByteString
    redis :: R.Redis a -> m a
