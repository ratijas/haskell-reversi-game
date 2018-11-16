{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module GameReversiServer.Types (
  Board (..),
  Cell (..),
  ErrorModel (..),
  Inline_response_200 (..),
  Inline_response_200_1 (..),
  Inline_response_200_2 (..),
  Inline_response_200_3 (..),
  Inline_response_200_3_players (..),
  Inline_response_200_4 (..),
  Location (..),
  UniqueLocations (..),
  User (..),
  ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 8 rows of board, A to H.
newtype Board = Board { unBoard :: List }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | Nullable boolean:   - null for empty cells;  - true for white cells;  - false for black cells. 
newtype Cell = Cell Bool deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
data ErrorModel = ErrorModel
  { errorModelMsg :: Text -- ^ Brief description of the error.
  } deriving (Show, Eq, Generic)

instance FromJSON ErrorModel where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "errorModel")
instance ToJSON ErrorModel where
  toJSON = genericToJSON (removeFieldLabelPrefix False "errorModel")

-- | 
data Inline_response_200 = Inline_response_200
  { inlineResponse200Token :: Text -- ^ Authorization token to be used with `Token` security definition. 
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse200")
instance ToJSON Inline_response_200 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse200")

-- | 
data Inline_response_200_1 = Inline_response_200_1
  { inlineResponse2001Players :: [User] -- ^ 
  , inlineResponse2001Invitations :: [User] -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200_1 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse2001")
instance ToJSON Inline_response_200_1 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse2001")

-- | 
data Inline_response_200_2 = Inline_response_200_2
  { inlineResponse2002Reply :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200_2 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse2002")
instance ToJSON Inline_response_200_2 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse2002")

-- | 
data Inline_response_200_3 = Inline_response_200_3
  { inlineResponse2003Status :: Text -- ^ Overall status of the game.   - turn/wait: your/rival's turn;  - win/lose: you win/lose;  - draw: nobody win, nobody lose, it's a draw.  - error: when you your opponent disconnects or timeouts. 
  , inlineResponse2003Players :: Inline_response_200_3_players -- ^ 
  , inlineResponse2003History :: UniqueLocations -- ^ 
  , inlineResponse2003Board :: Board -- ^ 
  , inlineResponse2003Available :: UniqueLocations -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200_3 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse2003")
instance ToJSON Inline_response_200_3 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse2003")

-- | 
data Inline_response_200_3_players = Inline_response_200_3_players
  { inlineResponse2003PlayersWhite :: User -- ^ 
  , inlineResponse2003PlayersBlack :: User -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200_3_players where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse2003Players")
instance ToJSON Inline_response_200_3_players where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse2003Players")

-- | 
data Inline_response_200_4 = Inline_response_200_4
  { inlineResponse2004Flipped :: UniqueLocations -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200_4 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse2004")
instance ToJSON Inline_response_200_4 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse2004")

-- | 
newtype Location = Location Text deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
newtype UniqueLocations = UniqueLocations { unUniqueLocations :: Location }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
data User = User
  { userUsername :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "user")
instance ToJSON User where
  toJSON = genericToJSON (removeFieldLabelPrefix False "user")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
