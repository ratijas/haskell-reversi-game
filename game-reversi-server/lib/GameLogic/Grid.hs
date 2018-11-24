{-# LANGUAGE OverloadedStrings #-}

module GameLogic.Grid (
  Board (..),
  Cord,
  Direction (..),
  allValidMoves,
  initBoard,
  ) where

import           Control.Applicative
import           Control.Monad       (guard)
import           Data.List           (any, concat)
import           Data.Map            (Map, fromList)
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Set            as Set
import           GameLogic.Disc      (Disc (..), swap)
import           GameLogic.Util

-- | Coordinate system goes from 0 to 7
-- (x, y)
type Cord = (Int, Int)

newtype Board = Board
  { unBoard :: Map Cord Disc
  } deriving (Show, Eq)

-- | Initial position of discs on the board.
initBoard :: Board
initBoard = Board $ Map.fromList
  [((3, 3), White),
   ((4, 4), White),
   ((3, 4), Black),
   ((4, 3), Black)]

-- | Orientation of the line
-- whether it is North, south east, west, south-east, etc
-- The order is important as it matches with the adjacent square list
data Direction = NW | N | NE | E | SE | S | SW | W
  deriving (Show, Eq, Enum)

  -- from direction to cord
dirToCord :: Direction -> Cord
dirToCord N  = (0, -1) :: Cord
dirToCord NE = (1, -1) :: Cord
dirToCord E  = (1, 0)  :: Cord
dirToCord SE = (1, 1)  :: Cord
dirToCord S  = (0, 1)  :: Cord
dirToCord SW = (-1, 1) :: Cord
dirToCord W  = (-1, 0) :: Cord
dirToCord NW = (-1, -1) :: Cord

-- | All possible coordinates on the board.
allCordinates :: [Cord]
allCordinates = (,) <$> [minX..maxX] <*> [minY..maxY]

-- | validate if the coordinate is inside the board
validate :: Cord -> Bool
validate (x, y) = not $ (x > maxX || x < minX) || (y > maxY || y < minY)

-- | Adjacent coordinates starting from NE clockwise
adjacent :: Cord -> [Cord]
adjacent (x, y) = Prelude.filter (\(a,b) -> a >= minX && a <= maxX
                                   && b >= minY && b <= maxY && (a,b) /= (x,y))
  $ (,) <$> [ x-1..x+1 ] <*> [ y-1..y+1 ]

direction :: Cord -> Cord -> Direction
direction (nc_x, nc_y) (oc_x, oc_y)
  | (nc_x > oc_x) && (nc_y > oc_y) = NW
  | (nc_x == oc_x) && (nc_y > oc_y) = N
  | (nc_x < oc_x) && (nc_y > oc_y) = NE
  | (nc_x < oc_x) && (nc_y == oc_y) = E
  | (nc_x < oc_x) && (nc_y < oc_y) = SE
  | (nc_x == oc_x) && (nc_y < oc_y) = S
  | (nc_x > oc_x) && (nc_y < oc_y) = SW
  | (nc_x > oc_x) && (nc_y == oc_y) = W
direction _ _ = error "Invalid direction"

-- | A move is valid if the cord is empty and its satisfy 2 requirements:
-- 1)there is an adjacent opposite disc
-- 2)it forms a sandwich with oppsite disc inside
isValidMove :: Board -> Disc -> Cord -> Bool
isValidMove board turn pos = (isEmptyCell board pos)
  && (not . null $ validAdjacent board turn pos)

-- | Is cell at given coordinate is empty?
isEmptyCell :: Board -> Cord -> Bool
isEmptyCell (Board board) pos = isNothing $ Map.lookup pos board

-- | Is a disc at given coordinate belongs to other player?
-- False if coordinate is empty.
isOpposite :: Board -> Disc -> Cord -> Bool
isOpposite (Board board) turn pos =
    (Map.lookup pos board) == (Just $ swap turn)

-- | All adjacent valid cord (here you can)
validAdjacent :: Board -> Disc -> Cord -> [Cord]
validAdjacent board turn pos = va
  where
    va :: [Cord]
    va = filter (\cord -> (isOpposite board turn cord) &&
                           (isSandwich board turn pos $ direction cord pos)
                )  (adjacent pos)

-- | Is there a disc with same color on given direction?
isSandwich :: Board -> Disc -> Cord -> Direction -> Bool
isSandwich (Board board) turn curpos dir =
  not . null $
    filter (\cord -> Map.lookup cord board == Just turn) $
      safeTail $
        line curpos dir

-- | Get nearest cord with same disc on given direction
nearestDisc :: Board -> Disc -> Cord -> Direction -> Cord
nearestDisc (Board board) turn pos dir =
  safeHead $ filter (\cord -> (Map.lookup cord board) == (Just turn)) $ safeTail $ line pos dir

updateBoard :: Board -> Disc -> Cord -> Maybe Board
updateBoard board turn pos = do
  guard $ isValidMove board turn pos
  return $ Board $ Map.union (fromList nboard) (unBoard board)
  where
    -- all valid adjacent to pos cords
    validAdj :: [Cord]
    validAdj = validAdjacent board turn pos
    -- pairs of valid adjacent cord and nearest cord with same disc
    validEnds :: [(Cord, Cord)]
    validEnds = map (\cord -> (cord, nearestDisc board turn pos $ direction cord pos) ) validAdj
    -- ass list
    nboard :: [(Cord, Disc)]
    nboard = case (validEnds) of
      []        -> []
      _ -> zip (concat $ map (\(x, y) -> between x y) validEnds) (repeat turn)

-- | Returns the sequence of squares from fist position to second position
-- including the start and end
between :: Cord -> Cord -> [Cord]
between pos1 pos2 =
  takeWhile (/= pos2) $ line pos1 $ direction pos1 pos2

-- | impl for Cord
cordPlus :: Cord -> Cord -> Cord
cordPlus (x1,y1) (x2,y2) = (x1+x2, y1+y2) :: Cord


-- | A sequence of squares from cord in direction
line :: Cord -> Direction -> [Cord]
line pos d = l
  where
    l = filter validate $ iterate (cordPlus $ dirToCord d) pos

-- | All possible directions.
allDirections :: [Direction]
allDirections = toEnum <$> [0..7::Int]

-- | Get all valid moves
allValidMoves :: Board -> Disc -> [Cord]
allValidMoves board turn = filter is_valid empties
  where
    is_valid = isValidMove board turn

    empties :: [Cord]
    empties = Set.toList $ Set.difference allSet filled
      where
        allSet = Set.fromList allCordinates
        filled = Set.fromList (fst <$> Map.toList (unBoard board))
