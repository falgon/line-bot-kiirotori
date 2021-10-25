{-# LANGUAGE OverloadedStrings #-}
module Main where
{-
import           LBKiirotori.AccessToken      (getAccessTokenIO, newConn)
import           LBKiirotori.API.PushMessage  (pushMessage)
import           LBKiirotori.Data.PushMessage

import           Control.Arrow                ((|||))
import           Control.Exception.Safe       (throwString)
import           Control.Monad                ((>=>))
import           Control.Monad.Except         (runExceptT)
import           Crypto.JWT                   (JWTError)
import qualified Data.ByteString              as B
import qualified Data.Text                    as T
import           Database.Redis               (Connection)
import           System.Environment           (getEnv)
-}

import           LBKiirotori.Webhook         (kiirotoriApp)
import           Network.Wai.Handler.Warp    (defaultSettings, setPort)
import           Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)

main :: IO ()
main = runTLS (tlsSettings "server.crt" "server.key") (setPort 48080 defaultSettings) kiirotoriApp

{-
    conn <- newConn
    token <- getAccessTokenIO conn
    userId <- getEnv "USER_ID"
    pushMessage token $ PushMessage {
        to = T.pack userId
      , messages = [
            MBText $ textMessage "pi!" Nothing Nothing
          ]
      }
-}
