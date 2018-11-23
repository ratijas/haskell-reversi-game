{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           GameReversiServer.API ( ServerConfig (..)
                                       , GameReversiServerAPI
                                       , toWarpSettings
                                       )
import qualified GameReversiServer.Authentication as Auth
import qualified GameReversiServer.Types          as Types
import qualified GameReversiServer.Handlers       as Handlers
import qualified GameReversiServer.Persist        as Persist

import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Applicative ((<$>), (<*>))
import Options.Applicative


-- * Servant tutorial
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Time.Calendar (Day, fromGregorian)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application, Server, serve, serveWithContext, serveDirectoryWebApp)
import Servant.API
import Servant.Server (Handler, ServantErr(..), err404, err503)
import System.Directory (doesFileExist)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)


type UserAPI1 = "users" :> Get '[JSON] [User]

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

server1 :: Server UserAPI1
server1 = return users1

userAPI1Proxy :: Proxy UserAPI1
userAPI1Proxy = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI1Proxy server1


type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

server2 :: Server UserAPI2
server2 = return users2
     :<|> return albert
     :<|> return isaac

userAPI2Proxy :: Proxy UserAPI2
userAPI2Proxy = Proxy

app2 :: Application
app2 = serve userAPI2Proxy server2


-- ** From combinators to handler arguments

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"


server3 :: Server API
server3 = position
     :<|> hello
     :<|> marketing

  where
    position :: Int -> Int -> Handler Position
    position x y = return $ Position { xCoord = x, yCoord = y }

    hello :: Maybe String -> Handler HelloMessage
    hello mname = return . HelloMessage $ case mname of
      Nothing -> "Hello, anonymous coward"
      Just n  -> "Hello, " ++ n

    marketing :: ClientInfo -> Handler Email
    marketing = return . emailForClient

userAPI3Proxy :: Proxy API
userAPI3Proxy = Proxy

app3 :: Application
app3 = serve userAPI3Proxy server3


-- ** Performing IO

type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

instance ToJSON FileContent

server5 :: Server IOAPI1
server5 = do
  filecontent <- liftIO (readFile "stack.yaml")
  return (FileContent filecontent)

_IOAPI1Proxy :: Proxy IOAPI1
_IOAPI1Proxy = Proxy

app5 ::Application
app5 = serve _IOAPI1Proxy server5

-- ** Failing, through `ServantErr`

failingHandler :: Handler ()
failingHandler = throwError myerr

  where myerr :: ServantErr
        myerr = err503 { errBody = "Sorry dear user." }

server6 :: Server IOAPI1
server6 = do
  exists <- liftIO (doesFileExist "myfile.txt")
  if exists
    then liftIO (readFile "myfile.txt") >>= return . FileContent
    else throwError custom404Err

  where custom404Err = err404 { errBody = "myfile.txt just isn't there, please leave this server alone." }

app6 = serve _IOAPI1Proxy server6

-- ** Response headers

type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myHandler :: Server MyHandler
myHandler = return $ addHeader 1797 albert

type MyHeadfulHandler = Get '[JSON] (Headers '[Header "X-A-Bool" Bool, Header "X-An-Int" Int] User)

myHeadfulHandler :: Server MyHeadfulHandler
myHeadfulHandler = return $ addHeader True $ addHeader 1797 albert

appHeadful :: Application
appHeadful = serve (Proxy :: Proxy MyHeadfulHandler) myHeadfulHandler

-- *** No Header

type MyMaybeHeaderHandler
  = Capture "withHeader" Bool :> Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myMaybeHeaderHandler :: Server MyMaybeHeaderHandler
myMaybeHeaderHandler x = return $ if x then addHeader 1797 albert
                                       else noHeader albert

appMaybeHeader :: Application
appMaybeHeader = serve (Proxy :: Proxy MyMaybeHeaderHandler) myMaybeHeaderHandler


-- ** Serving static content

type StaticAPI = "static" :> Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

server7 :: Server StaticAPI
server7 = serveDirectoryWebApp "static-files"

app7 :: Application
app7 = serve staticAPI server7


-- ** Nested APIs

type UserAPI3 = -- view the user with given userid, in JSON
                Capture "userid" Int :> Get '[JSON] User

           :<|> -- delete the user with given userid. empty response
                Capture "userid" Int :> DeleteNoContent '[JSON] NoContent

-- factor out userid

type UserAPI4 = Capture "userid" Int :> (    Get '[JSON] User
                                        :<|> DeleteNoContent '[JSON] NoContent
                                        )

-- Server UserAPI3 = (Int -> Handler User)
--              :<|> (Int -> Handler NoContent)

-- Server UserAPI4 = Int -> (    Handler User
--                          :<|> Handler NoContent
--                          )

server8 :: Server UserAPI3
server8 = getUser
     :<|> deleteUser

  where getUser :: Int -> Handler User
        getUser _userid = error "..."

        deleteUser :: Int -> Handler NoContent
        deleteUser _userid = error "..."

-- notice how getUser and deleteUser
-- have a different type! no argument anymore,
-- the argument directly goes to the whole Server
server9 :: Server UserAPI4
server9 userid = getUser
            :<|> deleteUser

  where getUser :: Handler User
        getUser = error "..."

        deleteUser :: Handler NoContent
        deleteUser = error "..."

app9 = serve (Proxy :: Proxy UserAPI4) server9





-- we just factor out the "users" path fragment
type API1 = "users" :>
  (    Get '[JSON] [User] -- user listing
  :<|> Capture "userid" Int :> Get '[JSON] User -- view a particular user
  )

-- we factor out the Request Body
type API2 = ReqBody '[JSON] User :>
  (    Get '[JSON] User -- just display the same user back, don't register it
  :<|> PostNoContent '[JSON] NoContent  -- register the user. empty response
  )

-- we factor out a Header
type API3 = Header "Authorization" Token :>
  (    Get '[JSON] SecretData -- get some secret data, if authorized
  :<|> ReqBody '[JSON] SecretData :> PostNoContent '[JSON] NoContent -- add some secret data, if authorized
  )

newtype Token = Token ByteString
newtype SecretData = SecretData ByteString





type UsersAPI =
       Get '[JSON] [User] -- list users
  :<|> ReqBody '[JSON] User :> PostNoContent '[JSON] NoContent -- add a user
  :<|> Capture "userid" Int :>
         ( Get '[JSON] User -- view a user
      :<|> ReqBody '[JSON] User :> PutNoContent '[JSON] NoContent -- update a user
      :<|> DeleteNoContent '[JSON] NoContent -- delete a user
         )

usersServer :: Server UsersAPI
usersServer = getUsers :<|> newUser :<|> userOperations

  where getUsers :: Handler [User]
        getUsers = error "..."

        newUser :: User -> Handler NoContent
        newUser = error "..."

        userOperations userid =
          viewUser userid :<|> updateUser userid :<|> deleteUser userid

          where
            viewUser :: Int -> Handler User
            viewUser = error "..."

            updateUser :: Int -> User -> Handler NoContent
            updateUser = error "..."

            deleteUser :: Int -> Handler NoContent
            deleteUser = error "..."


app10 :: Application
app10 = serve (Proxy :: Proxy UsersAPI) usersServer



type ProductsAPI =
       Get '[JSON] [Product] -- list products
  :<|> ReqBody '[JSON] Product :> PostNoContent '[JSON] NoContent -- add a product
  :<|> Capture "productid" Int :>
         ( Get '[JSON] Product -- view a product
      :<|> ReqBody '[JSON] Product :> PutNoContent '[JSON] NoContent -- update a product
      :<|> DeleteNoContent '[JSON] NoContent -- delete a product
         )

data Product = Product { productId :: Int }

productsServer :: Server ProductsAPI
productsServer = getProducts :<|> newProduct :<|> productOperations

  where getProducts :: Handler [Product]
        getProducts = error "..."

        newProduct :: Product -> Handler NoContent
        newProduct = error "..."

        productOperations productid =
          viewProduct productid :<|> updateProduct productid :<|> deleteProduct productid

          where
            viewProduct :: Int -> Handler Product
            viewProduct = error "..."

            updateProduct :: Int -> Product -> Handler NoContent
            updateProduct = error "..."

            deleteProduct :: Int -> Handler NoContent
            deleteProduct = error "..."






type CombinedAPI = "users" :> UsersAPI
              :<|> "products" :> ProductsAPI

server10 :: Server CombinedAPI
server10 = usersServer :<|> productsServer




-- API for values of type 'a'
-- indexed by values of type 'i'
type APIFor a i =
       Get '[JSON] [a] -- list 'a's
  :<|> ReqBody '[JSON] a :> PostNoContent '[JSON] NoContent -- add an 'a'
  :<|> Capture "id" i :>
         ( Get '[JSON] a -- view an 'a' given its "identifier" of type 'i'
      :<|> ReqBody '[JSON] a :> PutNoContent '[JSON] NoContent -- update an 'a'
      :<|> DeleteNoContent '[JSON] NoContent -- delete an 'a'
         )

-- Build the appropriate 'Server'
-- given the handlers of the right type.
serverFor :: Handler [a] -- handler for listing of 'a's
          -> (a -> Handler NoContent) -- handler for adding an 'a'
          -> (i -> Handler a) -- handler for viewing an 'a' given its identifier of type 'i'
          -> (i -> a -> Handler NoContent) -- updating an 'a' with given id
          -> (i -> Handler NoContent) -- deleting an 'a' given its id
          -> Server (APIFor a i)
serverFor = error "..."
-- implementation left as an exercise. contact us on IRC
-- or the mailing list if you get stuck!













reversiServer :: Server GameReversiServerAPI
reversiServer
  =    Handlers.sessionNew
  :<|> privateAPI
    where
      privateAPI user =
        (    Handlers.sessionCheck user
        :<|> Handlers.sessionList  user
        )


reversiApplication :: Application
reversiApplication = serveWithContext
  (Proxy :: Proxy GameReversiServerAPI)
  Auth.reversiServerContext
  reversiServer


-- * End of Servant tutorial

main :: IO ()
main = do
  config <- parseArguments
  let settings = toWarpSettings config
  Warp.runSettings settings reversiApplication

-- | Parse host and port from the command line arguments.
parseArguments :: IO ServerConfig
parseArguments =
  execParser $
    info (opts <**> helper) (progDesc "Game Reversi server")
    where
      opts = ServerConfig
        <$> strOption
            ( long "host"
           <> metavar "HOST"
           <> value "localhost"
           <> help "Host to serve on" )
        <*> option auto
            ( long "port"
           <> metavar "PORT"
           <> value 8080
           <> help "Port to serve on" )
