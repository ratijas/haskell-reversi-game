{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GameReversiServer.Persist
  (
    User( User )
  , nickname, score
  , loadUser
  , storeUser
  ) where

import           Control.Monad          (liftM)
import           Control.Monad.IO.Class (liftIO)
import           Text.Read              (readMaybe)
import           GHC.Generics           ( Generic )
import           Data.Aeson             ( ToJSON
                                        , FromJSON
                                        , encode
                                        , decodeStrict'
                                        )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Char8         as C8
import           Data.ByteString.Char8  ( ByteString
                                        , pack
                                        , unpack)
import           Database.Redis         ( Connection
                                        , Reply
                                        , runRedis
                                        , set
                                        , get
                                        , checkedConnect
                                        , defaultConnectInfo
                                        )


data User = User { nickname :: String -- ^ User identifier
                 , score :: Int       -- ^ In-game score
                 }
                 deriving Generic
instance ToJSON User
instance FromJSON User

-- | Connect to Redis server
getConnection :: IO Connection
getConnection = checkedConnect defaultConnectInfo

-- | Compose user key for Redis
userKey :: String -> ByteString
userKey name = pack ("user:" ++ name)

-- | Load user from a Redis database
loadUser :: String -> IO (Maybe User)
loadUser name = do
  conn <- getConnection
  runRedis conn $ do
    userEither <- get (userKey name) -- :: Either Reply (Maybe ByteString)
    return $ responseToUser userEither
  where
    responseToUser :: Either Reply (Maybe ByteString) -> Maybe User
    responseToUser (Right (Just bytes)) = bytesToUser bytes
    responseToUser _ = Nothing

    bytesToUser :: ByteString -> Maybe User
    bytesToUser bytes = do
      user <- decodeStrict' bytes
      return user

-- | Store user to a Redis database
storeUser :: User -> IO ()
storeUser user = do
  conn <- getConnection
  runRedis conn $ do
    _ <- set (userKey (nickname user)) (BL.toStrict (encode user))
    return ()
