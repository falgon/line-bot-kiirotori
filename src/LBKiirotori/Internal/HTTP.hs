{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.Internal.HTTP (
    reqMessage
  , reqGet
) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString      as B
import           Network.HTTP.Conduit (RequestBody (..), requestHeaders)
import           Network.HTTP.Simple

buildBearerAuth :: B.ByteString -> B.ByteString
buildBearerAuth = B.append "Bearer "

applyBearerAuth :: B.ByteString -> Request -> Request
applyBearerAuth bearerToken req = setRequestHeaders (authHeader : requestHeaders req) req
    where
        authHeader = ("Authorization", buildBearerAuth bearerToken)

setRequestBearerAuth :: B.ByteString -> Request -> Request
setRequestBearerAuth = applyBearerAuth

type EndPoint = String

reqMessage :: ToJSON a => EndPoint -> B.ByteString -> a -> Request
reqMessage endpoint token pm = setRequestMethod "POST"
    $ setRequestBearerAuth token
    $ setRequestBodyJSON pm
    $ parseRequest_ endpoint

reqGet :: EndPoint -> B.ByteString -> Request
reqGet endpoint token = setRequestMethod "GET"
    $ setRequestBearerAuth token
    $ parseRequest_ endpoint
