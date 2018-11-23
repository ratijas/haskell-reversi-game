module GameLogic.Disc where

import           Data.Text
import           GameLogic.Util

data Disc = White | Black
    deriving (Show, Eq, Ord)

-- | Swaps the turn
swap :: Disc -> Disc
swap White = Black
swap Black = White


isBlack = (== Black)
isWhite = not . isBlack

