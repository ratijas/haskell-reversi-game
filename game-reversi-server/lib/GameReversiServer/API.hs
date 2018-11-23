{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module GameReversiServer.API (
  ServerConfig (..),
  GameReversiServerAPI,
  toWarpSettings,
  ) where

import           GameReversiServer.Types
import qualified GameReversiServer.Authentication as A

import           Control.Monad.Except      (ExceptT)
import           Control.Monad.IO.Class
import           Data.Aeson                (Value, FromJSON, ToJSON)
import           Data.Coerce               (coerce)
import           Data.Function             ((&))
import qualified Data.Map as Map
import           Data.Monoid               ((<>))
import           Data.Proxy                (Proxy(..))
import qualified Data.Text as T
import           Data.Text                 (Text)
import           GHC.Exts                  (IsString(..))
import           GHC.Generics              (Generic)
import           Network.HTTP.Client       (Manager, defaultManagerSettings, newManager)
import           Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant                   (Application, Server, ServantErr, serve)
import           Servant.API
import           Servant.API.Verbs         (StdMethod(..), Verb)
import           Servant.Client            (Scheme(Http), ServantError, client)
import           Web.HttpApiData


-- | Servant type-level API, generated from the Swagger spec for GameReversiServer.
type GameReversiServerAPI
    =    "session" :> "new" :> Capture "username" Text :> Post '[JSON] ResponseSessionNew -- 'POST /session/new/{username}' route
    :<|> A.TokenAuth :>
        (    "session" :> "check" :> Get '[JSON] ResponseSessionCheck  -- 'GET /session/check' route
        :<|> "session" :> "list" :> Get '[JSON] ResponseSessionList    -- 'GET /session/list' route
        :<|> "session" :> "invite" :> Capture "username" Text :> Post '[JSON] ResponseSessionInvite -- 'POST /session/invite/{username}' route
        )
    -- :<|> "session" :> "invitation" :> Capture "reply" Text :> Capture "username" Text :> Verb 'POST 200 '[JSON] () -- 'sessionInvitationReplyUsernamePost' route
    -- :<|> "game" :> "reversi" :> "status" :> Verb 'GET 200 '[JSON] Inline_response_200_3 -- 'gameReversiStatusGet' route
    -- :<|> "game" :> "surrender" :> Verb 'POST 200 '[JSON] () -- 'gameSurrenderPost' route
    -- :<|> "game" :> "turn" :> Capture "location" Text :> Verb 'POST 200 '[JSON] Inline_response_200_4 -- 'gameTurnLocationPost' route


-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int     -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)


toWarpSettings :: ServerConfig -> Warp.Settings
toWarpSettings config =
  Warp.defaultSettings
    & Warp.setPort (configPort config)
    & Warp.setHost (fromString (configHost config))
