{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module GameReversiServer.Persist
  (
    User( User )
  , username
  , token
  , createUserIfNotExist
  , loadUser
  ) where

import           Control.Monad          ( guard, liftM )
import           Control.Monad.IO.Class ( liftIO )
import           GHC.Generics           ( Generic )
import           Data.Aeson             ( ToJSON
                                        , FromJSON
                                        , toJSON
                                        , parseJSON
                                        , encode
                                        , decodeStrict'
                                        , Value ( Object )
                                        , object
                                        , (.=)
                                        , (.:)
                                        )
import           Data.Aeson.Types       ( typeMismatch )

import           Data.Text              ( Text )
import qualified Data.Text as T
import           Data.Text.Encoding     ( decodeUtf8, encodeUtf8 )
import           Data.ByteString        ( ByteString )
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL (
                                          toStrict
                                        , fromStrict
                                        )

import           Database.Redis         ( Connection
                                        , Reply
                                        , runRedis
                                        , hget
                                        , hsetnx
                                        , checkedConnect
                                        , defaultConnectInfo
                                        )
import qualified Data.UUID    as UUID ( UUID, toByteString, fromByteString )
import qualified Data.UUID.V4 as UUID ( nextRandom )
import qualified GameReversiServer.Types as Types


data User = User
  { username :: Text      -- ^ Non-empty username
  , token    :: UUID.UUID -- ^ UUID v4 token
  }

-- | Connect to Redis server
getConnection :: IO Connection
getConnection = checkedConnect defaultConnectInfo

-- | Compose user key for Redis
userKey :: Text -> ByteString
userKey name = encodeUtf8 (T.append "user:" name)


err :: IO b
err = fail "Redis error"

constErr :: a -> IO b
constErr = const $ err


-- | Try to create new user with given username, return Nothing if one already exist.
createUserIfNotExist :: Text -> IO (Maybe User)
createUserIfNotExist (T.uncons -> Nothing) = return Nothing
createUserIfNotExist name = do
  conn <- getConnection
  uuid <- UUID.nextRandom
  let tok :: ByteString = BL.toStrict $ UUID.toByteString uuid
  (reply :: Either Reply Bool) <- runRedis conn $ do
    -- Hash set if not exist (key, field, value)
    hsetnx (userKey name) "token" tok
  (inserted :: Bool) <- either constErr return reply
  return $ if inserted then Just $ User name uuid
                       else Nothing


-- | Load user from a Redis database
loadUser
  :: Text            -- ^ username
  -> IO (Maybe User)
loadUser name = do
  conn <- getConnection
  (reply :: Either Reply (Maybe ByteString)) <- runRedis conn $ do
    -- Hash get (key, field)
    hget (userKey name) "token"
  (m :: Maybe ByteString) <- either constErr return reply
  case m of
    Nothing  -> return Nothing
    Just tok -> do
      (uuid :: UUID.UUID) <- maybe (fail "Invalid UUID in database") return $
        UUID.fromByteString (BL.fromStrict tok)
      return $ Just $ User name uuid
