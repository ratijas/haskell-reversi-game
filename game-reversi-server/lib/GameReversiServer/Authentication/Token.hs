{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module GameReversiServer.Authentication.Token where

import           Control.Monad      (guard)
import           Data.ByteString    (ByteString)
import qualified Data.List          as L
import           Data.Text          (Text, append, splitOn, )
import qualified Data.Text.Encoding as E
import qualified Data.UUID          as UUID


-- | Extract user credentials from `Authorization: Token <...>` HTTP header.
fromBytes :: ByteString -> Maybe (Text, UUID.UUID)
fromBytes bytes = do
  let (chunks :: [Text]) = splitOn ":" $ E.decodeUtf8 bytes
  guard $ L.length chunks == 2
  let [name, token] = chunks
  uuid <- UUID.fromText token
  return (name, uuid)


-- | Convert user credentials to token for `Authorization: Token <...>` HTTP header.
toBytes :: (Text, UUID.UUID) -> ByteString
toBytes (name, uuid) =
  E.encodeUtf8 $ append name $ UUID.toText uuid
