{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module GameReversiServer.Persist
  (
    User( User )
  , userToTypes
  , userFromTypes
  , username
  , token
  , online
  , createUserIfNotExist
  , loadUser
  , updateOnlineStatus
  , listOnlineUsers
  , listInvitations
  , invitationAccept
  , invitationReject
  , waitInvitation
  , gameInit
  , gameLoad
  ) where

import           Data.Maybe             ( catMaybes, isJust )
import           Control.Monad          ( guard, liftM, void )
import           Control.Monad.IO.Class ( liftIO )
import           Data.IORef             ( newIORef, readIORef, writeIORef )
import           Data.Text              ( Text )
import qualified Data.Text as T
import           Data.Text.Encoding as E
import           Data.ByteString        ( ByteString )
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL ( toStrict, fromStrict )

import qualified Database.Redis as R
import           Database.Redis         ( Connection
                                        , Reply
                                        , SetOpts (..)
                                        , setSeconds, setMilliseconds, setCondition
                                        , runRedis
                                        , get , setOpts
                                        , sadd, setnx
                                        , keys, del
                                        , checkedConnect, defaultConnectInfo
                                        )
import qualified Data.UUID    as UUID   ( UUID, toByteString, fromByteString )
import qualified Data.UUID.V4 as UUID   ( nextRandom )
import           System.Timeout         ( timeout )

import qualified GameReversiServer.Types as Types


-- * Redis internals
-- usernames                -- ^ Set of all users
-- token:{username}         -- ^ Authentication token
-- online:{username}        -- ^ On-line status,
--                          --   expire in 300 seconds (5 minutes)
-- invitations:{username}:* -- ^ All invitations for {username}
-- invitations:{to}:{from}  -- ^ Invitation for {to} from {from},
--                          --   expires in 30 seconds
-- invitation:accept:{to}   -- ^ Channels.
-- invitation:reject:{to}   --
--                          --   Message content: "{from}"
-- game:id:{username}       -- ^ A game that {username} currently playing.
--                          --   Value is the game id
-- game:info:{id}           -- ^ A hashmap with informations about the game.
--                          --   Fields:
--                          --   - player1: {username}
--                          --   - player2: {username}
--                          --   - history: JSON list of locations
--                          --   - board: JSON serialized board state

data User = User
  { username :: Text      -- ^ Non-empty username
  , token    :: UUID.UUID -- ^ UUID v4 token
  , online   :: Bool      -- ^ On-line status
  }

data Invitation = Invitation
  { invitationFrom :: User -- ^ User who initiated an invitation request
  , invitationTo   :: User -- ^ Invited user
  , invitationTTL  :: Int  -- ^ Time to live left
  }

-- | Handly contertator between modules. Strips all information, leaving only username.
userToTypes :: User -> Types.User
userToTypes User { username = name } = Types.User name

userFromTypes :: Types.User -> IO (Maybe User)
userFromTypes (Types.User name) =
  loadUser name

-- | Connect to Redis server
getConnection :: IO Connection
getConnection = checkedConnect defaultConnectInfo

-- | Redis usernames key set
usersKey :: ByteString
usersKey = "usernames"

-- | Compose user key for Redis
tokenKey
  :: Text -- ^ username
  -> ByteString
tokenKey name = E.encodeUtf8 (T.append "token:" name)

-- | Redis online: prefix
onlineKey
  :: Text -- ^ username
  -> ByteString
onlineKey name = E.encodeUtf8 (T.append "online:" name)


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
    -- add to the set, this is idempotent
    _ <- sadd usersKey [E.encodeUtf8 name]
    -- set if not exist (key, value)
    setnx (tokenKey name) tok
  (inserted :: Bool) <- either constErr return reply
  return $ do
    guard inserted
    return User
      { username = name
      , token = uuid
      , online = True
      }


-- | Load user from a Redis database
loadUser
  :: Text            -- ^ username
  -> IO (Maybe User)
loadUser name = do
  conn <- getConnection
  ((replyToken, replyOnline) :: (Either Reply (Maybe ByteString),
                                 Either Reply (Maybe ByteString))) <-
    runRedis conn $ do
      -- Hash get (key, field)
      token' <- get (tokenKey name)
      online' <- get (onlineKey name)
      return (token', online')
  (mToken :: Maybe ByteString) <- either constErr return replyToken
  (mOnline :: Maybe ByteString) <- either constErr return replyOnline
  case mToken of
    Nothing  -> return Nothing
    Just token' -> do
      (uuid :: UUID.UUID) <- maybe (fail "Invalid UUID in database") return $
        UUID.fromByteString (BL.fromStrict token')
      return $ Just User
        { username = name
        , token = uuid
        , online = isJust mOnline
        }


updateOnlineStatus :: User -> IO ()
updateOnlineStatus User { username = name, online = online' } = do
  conn <- getConnection
  let key = (onlineKey name)
  runRedis conn $ do
    if online'
      then do
        let opts = SetOpts{ setSeconds = Just 300
                          , setMilliseconds = Nothing
                          , setCondition = Nothing }
        _ <- setOpts key "" opts
        return ()
      else do
        _ <- del [key]
        return ()


keysToUsers
  :: ByteString   -- ^ Prefix
  -> [ByteString] -- ^ Keys with common prefix
  -> [Types.User]
keysToUsers prefix = map (Types.User . E.decodeUtf8) . catMaybes . map (B.stripPrefix prefix)

listOnlineUsers :: IO [Types.User]
listOnlineUsers = do
  conn <- getConnection
  (reply :: Either Reply [ByteString]) <- runRedis conn $ do
    keys "online:*"
  (keys' :: [ByteString]) <- either constErr return reply
  return $ keysToUsers "online:" keys'


listInvitations :: User -> IO [Types.User]
listInvitations User { username = name } = do
  conn <- getConnection
  let prefix = E.encodeUtf8 $ T.append "invitations:" $ T.append name ":"
  (reply :: Either Reply [ByteString]) <- runRedis conn $ do
    keys (B.append prefix "*")
  (keys' :: [ByteString]) <- either constErr return reply
  return $ keysToUsers prefix keys'


-- | accept and reject channels
invitationChannels :: User -> [ByteString]
invitationChannels User { username = name } =
  [ E.encodeUtf8 $ T.append "invitation:accept:" name
  , E.encodeUtf8 $ T.append "invitation:reject:" name
  ]

invitationsKey :: User -> User -> ByteString
invitationsKey to from =
  let to'   = username to
      from' = username from
    in E.encodeUtf8 $
      T.append "invitations:" $ T.append to' $ T.append ":" from'

invitationAccept :: User -> User -> IO ()
invitationAccept to from = do
  conn <- getConnection
  let [ch, _] = invitationChannels to
  let name = E.encodeUtf8 (username from)
  runRedis conn $ do
    void $ R.publish ch name

invitationReject :: User -> User -> IO ()
invitationReject to from = do
  conn <- getConnection
  let [_, ch] = invitationChannels to
  let name = E.encodeUtf8 (username from)
  runRedis conn $ do
    void $ R.publish ch name


waitInvitation
  :: User -- ^ to
  -> User -- ^ from
  -> IO Types.ResponseSessionInviteReply
waitInvitation to from = do
  r <- reply
  return $ case r of
    Just resp -> resp
    Nothing   -> Types.Reject
  where
    reply :: IO (Maybe Types.ResponseSessionInviteReply)
    reply = timeout (10 * 1000000) $ do
      -- timeout after 10 seconds
      conn <- getConnection
      reply' <- newIORef Types.Reject
      runRedis conn $ do
        let key = invitationsKey to from
        let opts = SetOpts{ setSeconds = Just 30
                          , setMilliseconds = Nothing
                          , setCondition = Nothing }
        void $ setOpts key "" opts
        R.pubSub (R.subscribe (invitationChannels to)) $ \msg -> do
            let ch = R.msgChannel msg
            putStrLn $ "Message from " ++ show (R.msgChannel msg) ++
                       ", content " ++ show (R.msgMessage msg)
            if E.decodeUtf8 (R.msgMessage msg) == username from
              then case ch of
                _ | B.isPrefixOf "invitation:accept:" ch -> do
                      writeIORef reply' Types.Accept
                      return $ R.unsubscribe []
                  | B.isPrefixOf "invitation:reject:" ch -> do
                      writeIORef reply' Types.Reject
                      return $ R.unsubscribe []
                  | otherwise                            -> do
                      fail "Unexpected message format"
              else return mempty

      readIORef reply'


gameIdKey :: User -> ByteString
gameIdKey = E.encodeUtf8 . T.append "game:id:" . username

gameInfoKey :: ByteString -> ByteString
gameInfoKey = B.append "game:info:"

gameIdValue :: User -> ByteString
gameIdValue = E.encodeUtf8 . username

getGameId :: User -> IO (Maybe ByteString)
getGameId user = do
  conn <- getConnection
  reply <- runRedis conn $ do
    R.get (gameIdKey user)
  either constErr return reply


gameInit :: User -> User -> IO ()
gameInit p1 p2 = do
  conn <- getConnection
  uuid <- UUID.nextRandom
  let gameId = BL.toStrict $ UUID.toByteString uuid

  runRedis conn $ do
    _ <- R.set (gameIdKey p1) gameId
    _ <- R.set (gameIdKey p2) gameId

    let infoK = (gameInfoKey gameId)
    _ <- R.hset infoK "player1" (E.encodeUtf8 $ username p1)
    _ <- R.hset infoK "player2" (E.encodeUtf8 $ username p2)
    _ <- R.hset infoK "history" ""
    --
    _ <- R.hset infoK "board"   ""

    return ()

gameLoad :: User -> IO (Maybe Types.ResponseGameStatus)
gameLoad user = do
  let user't = userToTypes user
  return $ Just $ Types.ResponseGameStatus
    { Types.responseGameStatus_status = Types.Turn
    , Types.responseGameStatus_players = Types.ResponseGameStatusPlayers user't user't
    , Types.responseGameStatus_board = Types.Board [[]]
    , Types.responseGameStatus_history = Types.UniqueLocations []
    , Types.responseGameStatus_available = Types.UniqueLocations []
    }
