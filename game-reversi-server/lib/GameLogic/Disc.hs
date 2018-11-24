module GameLogic.Disc (
  Disc (White, Black),
  swap,
  isBlack,
  isWhite,
  ) where

import Data.Text
import GameLogic.Util

data Disc = White | Black
    deriving (Show, Eq, Ord)

-- | Swaps the turn
swap :: Disc -> Disc
swap White = Black
swap Black = White

isBlack :: Disc -> Bool
isBlack = (== Black)

isWhite :: Disc -> Bool
isWhite = not . isBlack
