{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.Database.Redis.Keys (
    pinCodeKey
  , botUserIdKey
) where

import qualified Data.ByteString as B

pinCodeKey :: B.ByteString
pinCodeKey = "PINCODE"

botUserIdKey :: B.ByteString
botUserIdKey = "botUserId"
