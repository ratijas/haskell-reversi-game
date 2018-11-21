{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module GameReversiServer.API
  (
  -- * Server
    ServerConfig(..)
  , GameReversiServerBackend(..)
  , toWarpSettings
  -- , createGameReversiServerClient
  -- , runGameReversiServerServer
  -- ** Servant
  , GameReversiServerAPI
  ) where

import GameReversiServer.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application, Server, ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Web.HttpApiData


-- | Servant type-level API, generated from the Swagger spec for GameReversiServer.
type GameReversiServerAPI
    =    "game" :> "reversi" :> "status" :> Verb 'GET 200 '[JSON] Inline_response_200_3 -- 'gameReversiStatusGet' route
    :<|> "game" :> "surrender" :> Verb 'POST 200 '[JSON] () -- 'gameSurrenderPost' route
    :<|> "game" :> "turn" :> Capture "location" Text :> Verb 'POST 200 '[JSON] Inline_response_200_4 -- 'gameTurnLocationPost' route
    :<|> "session" :> "invitation" :> Capture "reply" Text :> Capture "username" Text :> Verb 'POST 200 '[JSON] () -- 'sessionInvitationReplyUsernamePost' route
    :<|> "session" :> "invite" :> Capture "username" Text :> Verb 'POST 200 '[JSON] Inline_response_200_2 -- 'sessionInviteUsernamePost' route
    :<|> "session" :> "list" :> Verb 'GET 200 '[JSON] Inline_response_200_1 -- 'sessionListGet' route
    :<|> "session" :> "new" :> Capture "username" Text :> Verb 'POST 200 '[JSON] Inline_response_200 -- 'sessionNewUsernamePost' route

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

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated  -- ^ CSV format for multiple parameters.
  | SpaceSeparated  -- ^ Also called "SSV"
  | TabSeparated    -- ^ Also called "TSV"
  | PipeSeparated   -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

-- | Backend for GameReversiServer.
-- The backend can be used both for the client and the server. The client generated from the GameReversiServer Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createGameReversiServerClient@). Alternatively, provided
-- a backend, the API can be served using @runGameReversiServerServer@.
data GameReversiServerBackend m = GameReversiServerBackend
  { gameReversiStatusGet :: m Inline_response_200_3{- ^ Get information about current status of the game, including:   - whose turn now  - who is playing white and who's black  - history of all turns  - whole board with discs  - available moves (if it's my turn now)  -}
  , gameSurrenderPost :: m (){- ^ Finish the game by surrendering to the opponent.  -}
  , gameTurnLocationPost :: Text -> m Inline_response_200_4{- ^  -}
  , sessionInvitationReplyUsernamePost :: Text -> Text -> m (){- ^  -}
  , sessionInviteUsernamePost :: Text -> m Inline_response_200_2{- ^ Requesting an invitation is a synchronous action. Upon receiving one, player has 10 seconds to reply. If he fail to do so, invitation is considered rejected.  -}
  , sessionListGet :: m Inline_response_200_1{- ^ This method does not work while game is on.  -}
  , sessionNewUsernamePost :: Text -> m Inline_response_200{- ^  -}
  }

-- -- | Run the GameReversiServer server at the provided host and port.
-- runGameReversiServerServer :: MonadIO m => ServerConfig -> GameReversiServerBackend (ExceptT ServantErr IO)  -> m ()
-- runGameReversiServerServer ServerConfig{..} backend =
--   liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy GameReversiServerAPI) (serverFromBackend backend)
--   where
--     warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
--     serverFromBackend GameReversiServerBackend{..} =
--       (coerce gameReversiStatusGet :<|>
--        coerce gameSurrenderPost :<|>
--        coerce gameTurnLocationPost :<|>
--        coerce sessionInvitationReplyUsernamePost :<|>
--        coerce sessionInviteUsernamePost :<|>
--        coerce sessionListGet :<|>
--        coerce sessionNewUsernamePost)
