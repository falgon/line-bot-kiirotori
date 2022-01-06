{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.API.Profile.FriendUser (
    profileFriendUser
) where

import           Control.Arrow                    ((|||))
import           Control.Arrow                    ((&&&))
import           Control.Exception.Safe           (Exception, MonadThrow (..),
                                                   throw, throwString)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString                  as B
import qualified Data.Text                        as T
import           GHC.Generics
import           Network.HTTP.Conduit             (RequestBody (..),
                                                   requestHeaders)
import           Network.HTTP.Simple

import           LBKiirotori.API.MessageErrorResp
import           LBKiirotori.Data.MessageObject
import           LBKiirotori.Database.Redis       (AccessToken (..))
import           LBKiirotori.Internal.HTTP        (reqProfile)

reqProfileFriendUser :: String -> B.ByteString -> Request
reqProfileFriendUser userId = reqProfile ("https://api.line.me/v2/bot/profile/" <> userId)

profileFriendUser :: (MonadThrow m, MonadIO m) => AccessToken -> String -> m ()
profileFriendUser token userId = do
    (statusCode, body) <- (getResponseStatusCode &&& (eitherDecode . getResponseBody))
        <$> httpLbs (reqProfileFriendUser userId (atToken token))
    if statusCode == 200 then pure ()
    else throwString ||| throw
        $ (body :: Either String MessageErrorResp)
