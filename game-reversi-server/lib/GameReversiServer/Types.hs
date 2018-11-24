{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module GameReversiServer.Types where

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
import           Servant.API      ( FromHttpApiData, parseUrlPiece )

import qualified GameLogic.Disc as Reversi
import qualified GameLogic.Grid as Reversi
import qualified GameLogic.Util as Reversi

-- | 8 rows of board, A to H. Each has 8 columns, 1 to 8.
newtype Board = Board { unBoard :: [[Int]] }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)


-- boardToGameLogic :: Board -> Reversi.Board
-- boardToGameLogic board =

boardFromGameLogic :: Reversi.Board -> Board
boardFromGameLogic rev =
  Board $ [row y | y <- [Reversi.minY..Reversi.maxY]]
  where
    row y = [cell y x | x <- [Reversi.minX..Reversi.maxX]]
    cell y x= fromDisc $ rev `Reversi.at` (x, y)

    fromDisc :: Maybe Reversi.Disc -> Int
    fromDisc Nothing = 0
    fromDisc (Just Reversi.White) = 1
    fromDisc (Just Reversi.Black) = -1


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

instance FromHttpApiData Location where
  parseUrlPiece text =
    let loc = Location text
      in case (locationToXY loc) of
        Just _ -> Right loc
        Nothing -> Left text


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


data ResponseGameStatus = ResponseGameStatus
  { responseGameStatus_status    :: GameStatus
  , responseGameStatus_players   :: ResponseGameStatusPlayers
  , responseGameStatus_board     :: Board
  , responseGameStatus_history   :: UniqueLocations
  , responseGameStatus_available :: UniqueLocations
  } deriving (Show, Eq, Generic)
instance FromJSON ResponseGameStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "responseGameStatus_")
instance ToJSON   ResponseGameStatus where
  toJSON    = genericToJSON    (removeFieldLabelPrefix "responseGameStatus_")


data GameStatus = Turn | Wait | Win | Lose | Draw | Error
  deriving (Show, Eq, Generic)
instance FromJSON GameStatus where
  parseJSON (A.String "turn" ) = return Turn
  parseJSON (A.String "wait" ) = return Wait
  parseJSON (A.String "win"  ) = return Win
  parseJSON (A.String "lose" ) = return Lose
  parseJSON (A.String "draw" ) = return Draw
  parseJSON (A.String "error") = return Error
  parseJSON invalid = typeMismatch "enum GameStatus" invalid
instance ToJSON   GameStatus where
  toJSON Turn  = "turn"
  toJSON Wait  = "wait"
  toJSON Win   = "win"
  toJSON Lose  = "lose"
  toJSON Draw  = "draw"
  toJSON Error = "error"


data ResponseGameStatusPlayers = ResponseGameStatusPlayers
  { responseGameStatusPlayers_white :: User
  , responseGameStatusPlayers_black :: User
  } deriving (Show, Eq, Generic)
instance FromJSON ResponseGameStatusPlayers where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "responseGameStatusPlayers_")
instance ToJSON   ResponseGameStatusPlayers where
  toJSON    = genericToJSON    (removeFieldLabelPrefix "responseGameStatusPlayers_")


data ResponseGameTurn = ResponseGameTurn
  { responseGameTurn_flipped :: UniqueLocations
  } deriving (Show, Eq, Generic)
instance FromJSON ResponseGameTurn where
  parseJSON = genericParseJSON (removeFieldLabelPrefix "responseGameTurn_")
instance ToJSON   ResponseGameTurn where
  toJSON    = genericToJSON    (removeFieldLabelPrefix "responseGameTurn_")


-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: String -> Options
removeFieldLabelPrefix prefix =
  defaultOptions {
    fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix
  }
