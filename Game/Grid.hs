{-# LANGUAGE OverloadedStrings #-}

module Grid where

import           Control.Applicative
import           Data.List           (Split, any, concat)
import           Data.Map            (Map, fromList)
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Set            as Set
import           Disc
import           Util

-- | Coordinate system goes from 0 to 7
type Cord = (Int, Int)


type Board = Map Cord Disc

-- | Orientation of the line
-- whether it is North, south east, west, south-east, etc
-- The order is important as it matches with the adjacent square list
data Direction = NW | N | NE | E | SE | S | SW | W
  deriving (Show, Eq, Enum)

DirToCoord :: Direction -> Cord
DirToCoord N  = Coord (0, -1)
DirToCoord NE = Coord (1, -1)
DirToCoord E  = Coord (1, 0)
DirToCoord SE = Coord (1, 1)
DirToCoord S  = Coord (0, 1)
DirToCoord SW = Coord (-1, 1)
DirToCoord W  = Coord (-1, 0)
DirToCoord NW = Coord (-1, -1)

-- array of coords
allCoordinates :: [(Int,Int)]
allCoordinates = (,) <$> [minX..maxX] <*> [minY..maxY]

-- validate if the coordinate is inside the board
validate :: Cord -> Maybe Cord
validate c@(x , y) = if (x > maxX || x < minX) || (y > maxY || y < minY)
  then Nothing else c

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

-- a move is valid if the coord is empty and its satisfy 2 requirements:
-- 1)there is an adjacent opposite disc
-- 2)it forms a sandwich with oppsite disc inside
isValidMove :: Cord -> Map Cord Disc -> Disc -> Bool
isValidMove pos board turn = isEmptySquare pos board
  && not . null $ validAdjacent board turn pos

-- is Coord is empty
isEmptySquare :: Cord -> Map Cord Disc -> Bool
isEmptySquare pos board = isNothing $ Map.lookup pos board

-- is Coord is opposite
isOpposite :: Map Coord Disc -> Disc -> Coord -> Bool
isOpposite board turn pos =
    Map.lookup pos board == Just $ swap turn

-- all adjacent valid coord (here you can)
validAdjacent :: Map Coord Disc -> Disc -> Coord -> [Coord]
validAdjacent board turn pos = va
  where
    va :: [Coord]
    va = filter (\coord -> isOpposite board turn coord &&
                           isSandwich board turn pos $ direction coord pos
                )  (adjacent pos)

-- is there a disc with same color on given direction
isSandwich :: Map Coord Disc -> Disc -> Coord -> Direction -> Bool
isSandwich board turn curpos dir =
  let res = not . null $ filter (\coord -> Map.lookup coord board == Just turn) $ safeTail $ line curpos dir
    in res

-- get nearest coord with same disc on given direction
nearestDisc :: Map Coord Disc -> Disc -> Coord -> Direction -> Coord
nearestDisc board turn pos dir =
  safeHead $ filter (\coord -> Map.lookup coord board == Just turn) $ safeTail $ line curpos dir

updateBoard :: Cord -> Disc -> Board -> Board
updateBoard pos turn board = Map.union (fromList nboard) board
  where
    -- ass list
    nboard :: [(Coord, Disc)]
    -- all valid adjacent to pos coords
    validAdj :: [Coord]
    validAdj = validAdjacent board turn pos
    -- pairs of valid adjacent coord and nearest coord with same disc
    validEnds :: [(Coord, Coord)]
    validEnds = map (\coord -> (coord, nearestDisc board turn $ direction coord pos) ) validAdj
    nboard = case (validEnds) of
      []        -> []
      otherwise -> zip (concat $ map (\(x, y) -> between x y) validEnds) turn

-- returns the sequence of squares from fist position to second position
-- including the start and end
between :: Cord -> Coord -> [Coord]
between pos1 pos2 =
  takeWhile (/= pos2) $ line pos1 $ direction pos1 pos2

-- + impl for Coord
coordPlus :: Coord -> Coord -> Coord
coordPlus (x1,y1) (x2,y2) = Coord (x1+x2, y1+y2)


--  returns a sequence of squares from cord in direction
line :: Cord -> Direction -> [Cord]
line pos d = l
  where
    l = filter (isJust . validate) $ iterate (coordPlus $ DirToCoord pos) pos

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
        allSet = Set.fromList allCoordinates
        filled = Set.fromList (fst <$> Map.toList board)
