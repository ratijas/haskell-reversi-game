{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameReversiServer.Authentication where

import           Control.Monad                    (guard)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson                       (ToJSON)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString     as B
import qualified Data.List           as L
import qualified Data.Map            as Map
import           Data.Map                         (Map, fromList)
import           Data.Monoid                      ((<>))
import           Data.Proxy                       (Proxy (Proxy))
import qualified Data.Text           as T
import           Data.Text                        (Text)
import           Data.Text.Encoding               ( decodeUtf8 )
import           Data.Typeable
import           GHC.Generics                     (Generic)
import           Network.Wai                      (Request, requestHeaders)
import           Network.Wai.Handler.Warp         (run)
import           Servant                          (throwError)
import           Servant.Server                   ( Application
                                        , BasicAuthCheck ( BasicAuthCheck )
                                        , BasicAuthResult( Authorized
                                                         , Unauthorized
                                                         )
                                        , Context( (:.)
                                                 , EmptyContext
                                                 )
                                        , Handler
                                        , Server
                                        , err401, err403, errBody
                                        , serve
                                        , serveWithContext
                                        )
import Servant.API                      ( (:<|>)( (:<|>) )
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
import Web.Cookie                       (parseCookies)


import qualified Data.UUID                 as UUID
import qualified GameReversiServer.Persist as Persist
import qualified GameReversiServer.Authentication.Token as Token


-- | private data that needs protection
newtype PrivateData = PrivateData { ssshhh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | public data that anyone can use.
newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

-- | A user we'll grab from the database when we authenticate someone
newtype User = User { username :: Text }
  deriving (Eq, Show)

-- | a type to wrap our public api
type PublicAPI = Get '[JSON] [PublicData]

-- | a type to wrap our private api
type PrivateAPI = Get '[JSON] PrivateData

-- | our API
type BasicAPI = "public"  :> PublicAPI
           :<|> "private" :> BasicAuth "foo-realm" User :> PrivateAPI

-- | a value holding a proxy of our API type
basicAuthApi :: Proxy BasicAPI
basicAuthApi = Proxy


-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: BasicAuthCheck User
authCheck =
  let user :: BasicAuthData -> IO (BasicAuthResult User)
      user (BasicAuthData name password) =
        if name == "servant" && password == "server"
        then return (Authorized (User "servant"))
        else return Unauthorized
  in BasicAuthCheck user

-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

basicAuthServer :: Server BasicAPI
basicAuthServer =
  let
    publicAPIHandler = return [PublicData "foo", PublicData "bar"]
    privateAPIHandler (user :: User) = return (PrivateData (username user))
  in publicAPIHandler :<|> privateAPIHandler


basicAuthApp :: Application
basicAuthApp = serveWithContext
  (Proxy :: Proxy BasicAPI)
  basicAuthServerContext
  basicAuthServer


-- * Generalized Authentication

-- | An account type that we "fetch from the database" after
-- performing authentication
newtype Account = Account { unAccount :: Text }

-- | A (pure) database mapping keys to accounts.
database :: Map ByteString Account
database = fromList [ ("key1", Account "Anne Briggs")
                    , ("key2", Account "Bruce Cockburn")
                    , ("key3", Account "Ghédalia Tazartès")
                    ]

-- | A method that, when given a password, will return an Account or throw an error.
-- This is our bespoke (and bad) authentication logic.
lookupAccount :: ByteString -> Handler Account
lookupAccount key = case Map.lookup key database of
  Nothing -> throwError (err403 { errBody = "Invalid Cookie" })
  Just user -> return user


--- | The auth handler wraps a function from Request -> Handler Account.
--- We look for a token in the request headers that we expect to be in the cookie.
--- The token is then passed to our `lookupAccount` function.
authHandler :: AuthHandler Request Account
authHandler = mkAuthHandler handler
  where
  maybeToEither e = maybe (Left e) Right
  throw401 msg = throwError $ err401 { errBody = msg }
  handler req = either throw401 lookupAccount $ do
    cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
    maybeToEither "Missing token in cookie" $ lookup "servant-auth-cookie" $ parseCookies cookie

-- | Our API, with auth-protection
type AuthGenAPI = "private" :> AuthProtect "cookie-auth" :> PrivateAPI
             :<|> "public"  :> PublicAPI

-- | A value holding our type-level API
genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = Account

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Context (AuthHandler Request Account ': '[])
genAuthServerContext = authHandler :. EmptyContext

-- | Our API, where we provide all the author-supplied handlers for each end
-- point. Note that 'privateDataFunc' is a function that takes 'Account' as an
-- argument. We dont' worry about the authentication instrumentation here,
-- that is taken care of by supplying context
genAuthServer :: Server AuthGenAPI
genAuthServer =
  let privateDataFunc (Account name) =
          return (PrivateData ("this is a secret: " <> name))
      publicData = return [PublicData "this is a public piece of data"]
  in  privateDataFunc :<|> publicData

genAuthApp :: Application
genAuthApp = serveWithContext
  (Proxy :: Proxy AuthGenAPI)
  genAuthServerContext
  genAuthServer



















-- | Perform authentication of user against the database
authenticateUser
  :: Text      -- ^ Username
  -> UUID.UUID -- ^ Authentication token as UUID
  -> IO (Maybe Persist.User)
authenticateUser name token = do
  (m :: Maybe Persist.User) <- Persist.loadUser name
  -- if user if loaded AND tokens are equal, then return User, otherwise return nothing.
  return $ case m of
    Just user@Persist.User { Persist.token = t } | t == token -> Just user
    _                                                         -> Nothing


-- | Token authentication via `Authorization: Token <...>` header.
tokenAuthHandler :: AuthHandler Request Persist.User
tokenAuthHandler = mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 { errBody = msg }
    throw403 msg = throwError $ err403 { errBody = msg }
    maybeToEither e = maybe (Left e) Right

    handler req = either throw401 lookupUser $ do
      (header :: ByteString) <- maybeToEither "Missing Authorization header" $
        lookup "Authorization" $ requestHeaders req
      (token :: ByteString) <- maybeToEither "Invalid Authorization header format" $
        getToken header
      (user, uuid) <- maybeToEither "Invalid token credentials" $
        Token.fromBytes token
      return (user, uuid)

    getToken :: ByteString -> Maybe ByteString
    getToken = B.stripPrefix "Token "

    lookupUser :: (Text, UUID.UUID)  -> Handler Persist.User
    lookupUser (name, uuid) = do
      authenticated <- liftIO $ authenticateUser name uuid
      case authenticated of
        Nothing   -> throw403 "Invalid credentials"
        Just user -> return user

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "token") = Persist.User


-- | The context that will be made available to request handlers. We supply the
-- "token"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
reversiServerContext :: Context (AuthHandler Request Persist.User ': '[])
reversiServerContext = tokenAuthHandler :. EmptyContext
