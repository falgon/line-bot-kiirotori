module Main where

import           Control.Monad.Except    (runExceptT)
import           Crypto.JWT              (JWTError)
import           LBKiirotori.AccessToken (LineReqResp, reqAccessToken)

main :: IO ()
main = do
    r <- runExceptT reqAccessToken :: IO (Either JWTError LineReqResp)
    print r
