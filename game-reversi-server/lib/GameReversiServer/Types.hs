{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module GameReversiServer.Types (
  Board (..),

  ErrorModel (..),
  Location (..),
  UniqueLocations (..),
  User (..),
  locationToXY,
  locationFromXY,

  ResponseSessionNew (..),
  ResponseSessionCheck (..),
  ResponseSessionList (..),
  ResponseSessionInvite (..),
  ResponseSessionInviteReply (..),

  Inline_response_200 (..),
  Inline_response_200_1 (..),
  Inline_response_200_2 (..),
  Inline_response_200_3 (..),
  Inline_response_200_3_players (..),
  Inline_response_200_4 (..),
  ) where

import           Data.Char        (ord, chr)
import           Data.List        (stripPrefix)
import           Data.Maybe       (fromMaybe)
import qualified Data.Aeson       as A
import           Data.Aeson       ( Value ( Object )
                                  , object
                                  , FromJSON(..)
                                  , ToJSON(..)
                                  , genericToJSON
                                  , genericParseJSON
                                  , (.:)
                                  , (.=)
                                  )
import           Data.Aeson.Types ( Options(..)
                                  , defaultOptions
                                  , typeMismatch
                                  )
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Map         as Map
import           GHC.Generics     (Generic)
import           Data.Function    ((&))
import qualified Data.UUID        ( UUID, toString, fromString )

-- | 8 rows of board, A to H.
newtype Board = Board { unBoard :: [Int] }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)


-- | Error Model
data ErrorModel = ErrorModel
  { errorModel_msg :: Text -- ^ Brief description of the error.
  } deriving (Show, Eq, Generic)
instance FromJSON ErrorModel where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "errorModel_")
instance ToJSON ErrorModel where
  toJSON    = genericToJSON    (removeFieldLabelPrefix "errorModel_")


-- | Client-side user model.
data User = User
  { user_username :: Text -- ^ unique username
  } deriving (Show, Eq, Generic)
instance FromJSON User where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "user_")
instance ToJSON User where
  toJSON    = genericToJSON    (removeFieldLabelPrefix "user_")


-- | Location on the board, described by two charactes: letter and digit
newtype Location = Location Text deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | Location like "E2" to pair of (x, y) integers, where x and y are both in range [0..7]
locationToXY :: Location -> Maybe (Int, Int)
locationToXY (Location xy)
  | T.length xy == 2 = (,) <$> (xCoord (T.head xy)) <*> (yCoord (T.last xy))
  | otherwise = Nothing
  where
    -- | 'ABCDEFGH' -> [0..7]
    xCoord :: Char -> Maybe Int
    xCoord c
      | 'A' <= c && c <= 'H' = Just ((ord c) - (ord 'A'))
      | otherwise            = Nothing

    -- | '12345678' -> [0..7]
    yCoord :: Char -> Maybe Int
    yCoord c
      | '1' <= c && c <= '8' = Just ((ord c) - (ord '1'))
      | otherwise            = Nothing

-- | Pair (x, y) to Location, where x and y are both in range [0..7]
locationFromXY :: (Int, Int) -> Maybe Location
locationFromXY (x, y) =
  if 0 <= x && x <= 7 &&
     0 <= y && y <= 7
    then
      let
        xChar = (chr (x + (ord 'A')))
        yChar = (chr (y + (ord '1')))
      in Just $ Location (T.cons xChar $ T.cons yChar "")
    else Nothing


-- | Collection of ordered unique locations.
-- This type exists solely for the purpose of documentation.
newtype UniqueLocations = UniqueLocations [Location]
  deriving (Show, Eq, FromJSON, ToJSON, Generic)


-- | Response to /session/new/{username}
newtype ResponseSessionNew = ResponseSessionNew
  { responseSessionNew_token :: Text -- ^ Authorization token to be used with `Token` security definition.
  } deriving (Show, Eq, Generic)
instance FromJSON ResponseSessionNew where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "responseSessionNew_")
instance ToJSON   ResponseSessionNew where
  toJSON    = genericToJSON    (removeFieldLabelPrefix "responseSessionNew_")


-- | Check session. This response should be protected by authenticating endpoint,
-- so field "ok" will always be True.
newtype ResponseSessionCheck = ResponseSessionCheck
  { responseSessionCheck_ok :: Bool
  } deriving (Show, Eq, Generic)
instance FromJSON ResponseSessionCheck where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "responseSessionCheck_")
instance ToJSON   ResponseSessionCheck where
  toJSON    = genericToJSON    (removeFieldLabelPrefix "responseSessionCheck_")


-- | All on-line players AND pending invitations.
-- This method does not work while game is on.
data ResponseSessionList = ResponseSessionList
  { responseSessionList_players :: [User]     -- ^ on-line players
  , responseSessionList_invitations :: [User] -- ^ invitations from those players
  } deriving (Show, Eq, Generic)
instance FromJSON ResponseSessionList where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "responseSessionList_")
instance ToJSON   ResponseSessionList where
  toJSON    = genericToJSON    (removeFieldLabelPrefix "responseSessionList_")


data ResponseSessionInvite = ResponseSessionInvite
  { responseSessionInvite_reply :: ResponseSessionInviteReply
  } deriving (Show, Eq, Generic)
instance FromJSON ResponseSessionInvite where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "responseSessionInvite_")
instance ToJSON   ResponseSessionInvite where
  toJSON    = genericToJSON    (removeFieldLabelPrefix "responseSessionInvite_")

data ResponseSessionInviteReply
  = Accept
  | Reject
  deriving (Show, Eq, Generic)
instance FromJSON ResponseSessionInviteReply where
  parseJSON (A.String "accepted") = return Accept
  parseJSON (A.String "rejected") = return Reject
  parseJSON invalid = typeMismatch "enum Reply" invalid
instance ToJSON   ResponseSessionInviteReply where
  toJSON Accept = "accepted"
  toJSON Reject = "rejected"












-- |
data Inline_response_200 = Inline_response_200
  { inlineResponse200Token :: Text -- ^ Authorization token to be used with `Token` security definition.
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "inlineResponse200")
instance ToJSON Inline_response_200 where
  toJSON = genericToJSON (removeFieldLabelPrefix "inlineResponse200")

-- |
data Inline_response_200_1 = Inline_response_200_1
  { inlineResponse2001Players :: [User] -- ^
  , inlineResponse2001Invitations :: [User] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200_1 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "inlineResponse2001")
instance ToJSON Inline_response_200_1 where
  toJSON = genericToJSON (removeFieldLabelPrefix "inlineResponse2001")

-- |
data Inline_response_200_2 = Inline_response_200_2
  { inlineResponse2002Reply :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200_2 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "inlineResponse2002")
instance ToJSON Inline_response_200_2 where
  toJSON = genericToJSON (removeFieldLabelPrefix "inlineResponse2002")

-- |
data Inline_response_200_3 = Inline_response_200_3
  { inlineResponse2003Status :: Text -- ^ Overall status of the game.   - turn/wait: your/rival's turn;  - win/lose: you win/lose;  - draw: nobody win, nobody lose, it's a draw.  - error: when you your opponent disconnects or timeouts.
  , inlineResponse2003Players :: Inline_response_200_3_players -- ^
  , inlineResponse2003History :: UniqueLocations -- ^
  , inlineResponse2003Board :: Board -- ^
  , inlineResponse2003Available :: UniqueLocations -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200_3 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "inlineResponse2003")
instance ToJSON Inline_response_200_3 where
  toJSON = genericToJSON (removeFieldLabelPrefix "inlineResponse2003")

-- |
data Inline_response_200_3_players = Inline_response_200_3_players
  { inlineResponse2003PlayersWhite :: User -- ^
  , inlineResponse2003PlayersBlack :: User -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200_3_players where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "inlineResponse2003Players")
instance ToJSON Inline_response_200_3_players where
  toJSON = genericToJSON (removeFieldLabelPrefix "inlineResponse2003Players")

-- |
data Inline_response_200_4 = Inline_response_200_4
  { inlineResponse2004Flipped :: UniqueLocations -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200_4 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "inlineResponse2004")
instance ToJSON Inline_response_200_4 where
  toJSON = genericToJSON (removeFieldLabelPrefix "inlineResponse2004")





-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: String -> Options
removeFieldLabelPrefix prefix =
  defaultOptions {
    fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix
  }
