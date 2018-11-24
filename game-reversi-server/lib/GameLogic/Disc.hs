module GameLogic.Disc (
  Disc (White, Black),
  flip',
  isBlack,
  isWhite,
  ) where

import Data.Text
import GameLogic.Util

data Disc = White | Black
    deriving (Show, Eq, Ord)

-- | Flip the disc
flip' :: Disc -> Disc
flip' White = Black
flip' Black = White

isBlack :: Disc -> Bool
isBlack = (== Black)

isWhite :: Disc -> Bool
isWhite = not . isBlack
