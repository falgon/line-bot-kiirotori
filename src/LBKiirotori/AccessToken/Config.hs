module LBKiirotori.AccessToken.Config (
    AccessToken (..)
) where

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import           Data.Time.Clock (UTCTime)

data AccessToken = AccessToken {
    atKeyId     :: T.Text
  , atToken     :: BS.ByteString
  , atExpiresIn :: UTCTime
  } deriving Show
