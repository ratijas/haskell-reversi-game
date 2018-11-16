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
-fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=328 #-}

module GameReversiServer.API
  -- * Client and Server
  ( ServerConfig(..)
  , GameReversiServerBackend
  , createGameReversiServerClient
  , runGameReversiServerServer
  , runGameReversiServerClient
  , runGameReversiServerClientWithManager
  , GameReversiServerClient
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
import Servant (ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Web.HttpApiData




-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either String b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> (T.unpack key) <> " in form data"
    Just value ->
      case parseQueryParam value of
        Left result -> Left $ T.unpack result
        Right result -> Right $ result

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
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


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

newtype GameReversiServerClient a = GameReversiServerClient
  { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a
  } deriving Functor

instance Applicative GameReversiServerClient where
  pure x = GameReversiServerClient (\_ _ -> pure x)
  (GameReversiServerClient f) <*> (GameReversiServerClient x) =
    GameReversiServerClient (\manager url -> f manager url <*> x manager url)

instance Monad GameReversiServerClient where
  (GameReversiServerClient a) >>= f =
    GameReversiServerClient (\manager url -> do
      value <- a manager url
      runClient (f value) manager url)

instance MonadIO GameReversiServerClient where
  liftIO io = GameReversiServerClient (\_ _ -> liftIO io)

createGameReversiServerClient :: GameReversiServerBackend GameReversiServerClient
createGameReversiServerClient = GameReversiServerBackend{..}
  where
    ((coerce -> gameReversiStatusGet) :<|>
     (coerce -> gameSurrenderPost) :<|>
     (coerce -> gameTurnLocationPost) :<|>
     (coerce -> sessionInvitationReplyUsernamePost) :<|>
     (coerce -> sessionInviteUsernamePost) :<|>
     (coerce -> sessionListGet) :<|>
     (coerce -> sessionNewUsernamePost)) = client (Proxy :: Proxy GameReversiServerAPI)

-- | Run requests in the GameReversiServerClient monad.
runGameReversiServerClient :: ServerConfig -> GameReversiServerClient a -> ExceptT ServantError IO a
runGameReversiServerClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runGameReversiServerClientWithManager manager clientConfig cl

-- | Run requests in the GameReversiServerClient monad using a custom manager.
runGameReversiServerClientWithManager :: Manager -> ServerConfig -> GameReversiServerClient a -> ExceptT ServantError IO a
runGameReversiServerClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the GameReversiServer server at the provided host and port.
runGameReversiServerServer :: MonadIO m => ServerConfig -> GameReversiServerBackend (ExceptT ServantErr IO)  -> m ()
runGameReversiServerServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy GameReversiServerAPI) (serverFromBackend backend)
  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend GameReversiServerBackend{..} =
      (coerce gameReversiStatusGet :<|>
       coerce gameSurrenderPost :<|>
       coerce gameTurnLocationPost :<|>
       coerce sessionInvitationReplyUsernamePost :<|>
       coerce sessionInviteUsernamePost :<|>
       coerce sessionListGet :<|>
       coerce sessionNewUsernamePost)
