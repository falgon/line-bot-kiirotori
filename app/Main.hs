{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow                ((|||))
import           Control.Exception.Safe       (throwString)
import           Control.Monad.Except         (runExceptT)
import           Crypto.JWT                   (JWTError)
import           Data.String                  (IsString (..))
import qualified Data.Text                    as T
import           LBKiirotori.AccessToken      (LineReqResp (..), reqAccessToken)
import           LBKiirotori.API.PushMessage  (pushMessage)
import           LBKiirotori.Data.PushMessage
import           System.Environment           (getEnv)

main :: IO ()
main = do
    r <- (throwString . show) ||| pure =<< (runExceptT reqAccessToken :: IO (Either JWTError LineReqResp))
    userId <- getEnv "USER_ID"
    pushMessage (fromString $ T.unpack $ accessToken r) $ PushMessage {
        to = T.pack userId
      , messages = [
            MBText $ textMessage "pi!" Nothing Nothing
          ]
      }
