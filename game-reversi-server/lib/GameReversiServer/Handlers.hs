{-# LANGUAGE OverloadedStrings #-}

module GameReversiServer.Handlers (
  sessionNew,
  sessionList,
  sessionInvite,
  sessionInvitationReply,
  gameStatus,
  gameTurn,
  gameSurrender,
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)

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
import Data.UUID as UUID                ( toText )

import qualified GameReversiServer.Types   as T
import qualified GameReversiServer.Persist as Persist

-- | Create a brand new session.
-- Note that session and username are considered inseparable.
--
-- * Algorithm:
--   1. tell Redis to setnx (set if not exist) a new token
--   2.a. if setnx inserted new record, a User object with a token is returned
--   2.b. otherwise throw an error 409: "Conflict. Username is already taken."
sessionNew :: Text -> Handler T.ResponseSessionNew
sessionNew username = do
  m <- liftIO $ Persist.createUserIfNotExist username
  maybe err ok m
  where
    ok = return . T.ResponseSessionNew . UUID.toText . Persist.token
    err = throwError $ err409
      { errBody = "Conflict. Username is already taken." }

sessionList :: ()
sessionList = ()

sessionInvite :: ()
sessionInvite = ()

sessionInvitationReply :: ()
sessionInvitationReply = ()

gameStatus :: ()
gameStatus = ()

gameTurn :: ()
gameTurn = ()

gameSurrender :: ()
gameSurrender = ()
