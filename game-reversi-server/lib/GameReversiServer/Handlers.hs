{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameReversiServer.Handlers (
  sessionNew,
  sessionCheck,
  sessionList,
  sessionInvite,
  sessionInvitationReply,
  gameStatus,
  gameTurn,
  gameSurrender,
  ) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text           as T
import           Data.Text              (Text)
import qualified Data.Text.Encoding  as E
import           Data.UUID as UUID      ( toText )
import           Servant                ( throwError )
import           Servant.Server         ( Application
                                        , BasicAuthCheck ( BasicAuthCheck )
                                        , BasicAuthResult( Authorized
                                                         , Unauthorized
                                                         )
                                        , Context( (:.)
                                                 , EmptyContext
                                                 )
                                        , Handler
                                        , Server
                                        , err401
                                        , err403
                                        , err404
                                        , err409
                                        , errBody
                                        , serve
                                        , serveWithContext
                                        )
import           Servant.API            ( (:<|>)((:<|>))
                                        , (:>)
                                        , AuthProtect
                                        , Get
                                        , JSON
                                        )
import Servant.API.BasicAuth            ( BasicAuth
                                        , BasicAuthData( BasicAuthData ))
import Servant.Server.Experimental.Auth ( AuthHandler
                                        , AuthServerData
                                        , mkAuthHandler
                                        )
import qualified GameReversiServer.Types                  as Types
import qualified GameReversiServer.Persist                as P
import qualified GameReversiServer.Authentication.Token   as Token

-- | Create a brand new session.
-- Note that session and username are considered inseparable.
--
-- * Algorithm:
--   1. tell Redis to setnx (set if not exist) a new token
--   2.a. if setnx inserted new record, a User object with a token is returned
--   2.b. otherwise throw an error 409: "Conflict. Username is already taken."
sessionNew :: Text -> Handler Types.ResponseSessionNew
sessionNew username = do
  m <- liftIO $ P.createUserIfNotExist username
  maybe err ok m
  where
    ok = return . Types.ResponseSessionNew . E.decodeUtf8 . Token.fromUser
    err = throwError $ err409
      { errBody = "Conflict. Username is already taken." }

sessionCheck :: P.User -> Handler Types.ResponseSessionCheck
sessionCheck _ = return $ Types.ResponseSessionCheck True

-- | Get on-line users AND pending invitations.
-- This method does not work while game is on.
--
-- * Algorithm:
--   0. Check that user is currently not playing a game, otherwise it's an error.
--   1. Update on-line status of requesting user.
--   2. Fetch all users with on-line status.
--   3. Fetch all invitations for requesting user.
sessionList :: P.User -> Handler Types.ResponseSessionList
sessionList user = do
  let user' = user { P.online = True }
  liftIO $ P.updateOnlineStatus user'
  onlines <- liftIO $ P.listOnlineUsers
  invitations <- liftIO $ P.listInvitations user'
  return $ Types.ResponseSessionList onlines invitations

-- | Synchronous invitation request.
-- This method does not work while game is on.
--
-- * Algorithm:
--   0. Check that other player exists, otherwise it's 404 error.
--   1. Blocking wait for reply with timeout of 10 seconds.
sessionInvite
  :: P.User -- ^ Authenticated user who initiated an invitation
  -> Text   -- ^ Invited user's username
  -> Handler Types.ResponseSessionInvite
sessionInvite user other'name = do
  (other'm :: Maybe P.User) <- liftIO $ P.loadUser other'name
  maybe notFound next other'm
  where
    notFound = throwError $ err404
      { errBody = "Invited player not found" }
    next :: P.User -> Handler Types.ResponseSessionInvite
    next other = do
      reply <- liftIO $ P.waitInvitation other user
      return $ Types.ResponseSessionInvite reply

sessionInvitationReply :: ()
sessionInvitationReply = ()

gameStatus :: ()
gameStatus = ()

gameTurn :: ()
gameTurn = ()

gameSurrender :: ()
gameSurrender = ()
