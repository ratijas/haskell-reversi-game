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
                                        , NoContent (..)
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
import qualified GameLogic.Grid                           as Reversi
import qualified GameLogic.Disc                           as Reversi
import qualified GameLogic.Util                           as Reversi

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

-- | Reply to an invitation request.
-- This method does not work while game is on.
--
-- * Algorithm:
--   0. Preconditions:
--   0.a. Check that other player exists, otherwise it's 404 error.
--   0.b. Check that invitation exists from other player and its TTL > 3 seconds.
--   1.a. If accepting:
--   1.a.1. Accept an invitation from this player.
--   1.a.2. Reject all other pending invitations.
--   1.a.3. Transition into the game state:
--   1.a.3.1. Unset both players' online status (so that nobody could send them an invitation).
--   1.a.3.2. Prepare game board.
--   1.b. Otherwise, rejecting:
--   1.b.1. Reject only invitation from this player.
sessionInvitationReply
  :: P.User                           -- ^ Authenticated user who replies to an invitation
  -> Types.ResponseSessionInviteReply -- ^ Particular reply
  -> Text                             -- ^ User who initiated an initiated request
  -> Handler NoContent
sessionInvitationReply user reply other'name = do
  (other'm :: Maybe P.User) <- liftIO $ P.loadUser other'name
  (other :: P.User) <- maybe userNotFound return other'm
  let other't = P.userToTypes other
  invitations <- liftIO $ P.listInvitations user
  if other't `elem` invitations then return ()
                                else invitationNotFound
  case reply of
    -- 1.a.
    Types.Accept -> do
      liftIO $ P.invitationAccept user other
      -- do
      --   u <- P.userFromTypes <$> ( invitations \\ (Types.User other'name))
      --   u' <- u
      --   liftIO $ P.invitationReject u'
      -- P.updateOnlineStatus user
      liftIO $ P.gameInit user other

    -- 1.b.
    Types.Reject -> do
      liftIO $ P.invitationReject user other

  return NoContent

  where
    userNotFound = throwError $ err404
      { errBody = "Player not found" }
    invitationNotFound = throwError $ err404
      { errBody = "Invitation not found" }



gameStatus :: P.User -> Handler Types.ResponseGameStatus
gameStatus user = do
  game' <- liftIO $ P.gameLoad user
  game <- maybe gameNotFound return game'
  return game

gameNotFound :: Handler a
gameNotFound = throwError $ err404
  { errBody = "Game not found" }

gameTurn
  :: P.User         -- ^ Authenticated user
  -> Types.Location -- ^ Location
  -> Handler Types.ResponseGameTurn
gameTurn user location = do
  game' <- liftIO $ P.gameLoad user
  game <- maybe gameNotFound return game'


  -- Reversi.

  return $ Types.ResponseGameTurn $ Types.UniqueLocations []


gameSurrender :: ()
gameSurrender = ()
