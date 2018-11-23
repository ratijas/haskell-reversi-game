{-# LANGUAGE OverloadedStrings #-}

module GameLogic.Grid where

import           Control.Applicative
import           Data.List           (any, concat)
import           Data.Map            (Map, fromList)
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Set            as Set
import           GameLogic.Disc
import           GameLogic.Util

-- | Coordinate system goes from 0 to 7
type Cord = (Int, Int)


type Board = Map Cord Disc

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

-- array of coords
allCordinates :: [(Int,Int)]
allCordinates = (,) <$> [minX..maxX] <*> [minY..maxY]

-- validate if the coordinate is inside the board
validate :: Cord -> Maybe Cord
validate c@(x , y) = if (x > maxX || x < minX) || (y > maxY || y < minY)
  then Nothing else (Just c)

-- return the adjacent coordinates starting from NE clockwise
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

-- a move is valid if the cord is empty and its satisfy 2 requirements:
-- 1)there is an adjacent opposite disc
-- 2)it forms a sandwich with oppsite disc inside
isValidMove :: Cord -> Map Cord Disc -> Disc -> Bool
isValidMove pos board turn = (isEmptySquare pos board)
  && (not . null $ validAdjacent board turn pos)

-- is Cord is empty
isEmptySquare :: Cord -> Map Cord Disc -> Bool
isEmptySquare pos board = isNothing $ Map.lookup pos board

-- is Cord is opposite
isOpposite :: Map Cord Disc -> Disc -> Cord -> Bool
isOpposite board turn pos =
    (Map.lookup pos board) == (Just $ swap turn)

-- all adjacent valid cord (here you can)
validAdjacent :: Map Cord Disc -> Disc -> Cord -> [Cord]
validAdjacent board turn pos = va
  where
    va :: [Cord]
    va = filter (\cord -> (isOpposite board turn cord) &&
                           (isSandwich board turn pos $ direction cord pos)
                )  (adjacent pos)

-- is there a disc with same color on given direction
isSandwich :: Map Cord Disc -> Disc -> Cord -> Direction -> Bool
isSandwich board turn curpos dir =
  let res = not . null $ filter (\cord -> Map.lookup cord board == Just turn) $ safeTail $ line curpos dir
    in res

-- get nearest cord with same disc on given direction
nearestDisc :: Map Cord Disc -> Disc -> Cord -> Direction -> Cord
nearestDisc board turn pos dir =
  safeHead $ filter (\cord -> (Map.lookup cord board) == (Just turn)) $ safeTail $ line pos dir

updateBoard :: Cord -> Disc -> Board -> Board
updateBoard pos turn board = Map.union (fromList nboard) board
  where
    -- ass list
    nboard :: [(Cord, Disc)]
    -- all valid adjacent to pos cords
    validAdj :: [Cord]
    validAdj = validAdjacent board turn pos
    -- pairs of valid adjacent cord and nearest cord with same disc
    validEnds :: [(Cord, Cord)]
    validEnds = map (\cord -> (cord, nearestDisc board turn pos $ direction cord pos) ) validAdj
    nboard = case (validEnds) of
      []        -> []
      _ -> zip (concat $ map (\(x, y) -> between x y) validEnds) (repeat turn)

-- returns the sequence of squares from fist position to second position
-- including the start and end
between :: Cord -> Cord -> [Cord]
between pos1 pos2 =
  takeWhile (/= pos2) $ line pos1 $ direction pos1 pos2

-- + impl for Cord
cordPlus :: Cord -> Cord -> Cord
cordPlus (x1,y1) (x2,y2) = (x1+x2, y1+y2) :: Cord


--  returns a sequence of squares from cord in direction
line :: Cord -> Direction -> [Cord]
line pos d = l
  where
    l = filter (isJust . validate) $ iterate (cordPlus $ dirToCord d) pos

allDirections :: [Direction]
allDirections = (toEnum <$> [0..7::Int])::[Direction]

--  get all valid moves
allValidMoves :: Board -> Disc -> [Cord]
allValidMoves board turn = filter is_valid empties
  where
    empties = emptyCords board
    is_valid c =  isValidMove c board turn

    emptyCords :: Board -> [Cord]
    emptyCords board = Set.toList $ Set.difference allSet filled
      where
        allSet = Set.fromList allCordinates
        filled = Set.fromList (fst <$> Map.toList board)
