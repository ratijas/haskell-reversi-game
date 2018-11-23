{-# LANGUAGE OverloadedStrings #-}

module Grid where

import           Control.Applicative
-- import           Control.Arrow
import           Data.List           (Split, any)
import           Data.Map            (Map, fromList)
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Set            as Set
import qualified Data.Text           as T
-- import           Debug.Trace
import           Disc
import           Util

-- | Coordinate system goes from 0 to 7
type Cord = (Int, Int)


type Board = Map Cord Disc

data User = User { username :: String }

data State = State {
  board :: Board,
  turn :: Maybe User,  -- ^ Current turn
  players :: Map Disc User,
  winner :: Maybe User,
  error :: Maybe String
}

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

-- | Gives the next co-ordinate in the given direction
move :: Direction -> Cord -> Maybe Cord
move N (x,y)  = validate (x, y-1)
move NE (x,y) = validate (x+1,y-1)
move E (x,y)  = validate (x+1,y)
move SE (x,y) = validate (x+1,y+1)
move S (x,y)  = validate (x,y+1)
move SW (x,y) = validate (x-1,y+1)
move W (x,y)  = validate (x-1,y)
move NW (x,y) = validate (x-1,y-1)

-- | It is a valid move if
-- 1) The current pos is empty
-- 2) There is an adjacent square with opposite colored disc
-- 3) placing the disc creates a sandwich
isValidMove :: Cord -> Map Cord Disc -> Disc -> Bool
isValidMove pos board turn = isEmptySquare pos board
  && not . null $ validAdjacent board turn pos

-- | Condition 1) in @isValidMove@
isEmptySquare :: Cord -> Map Cord Disc -> Bool
isEmptySquare pos board = isNothing $ Map.lookup pos board

-- 2) Condition in @isValidMove@
isOpposite :: Map Coord Disc -> Disc -> Coord -> Bool
isOpposite board turn pos =
    Map.lookup pos board == Just $ swap turn

validAdjacent :: Map Coord Disc -> Disc -> Coord -> [Coord]
validAdjacent board turn pos = va
  where
    va :: [Coord]
    va = filter (\coord -> isOpposite board turn coord &&
                           isSandwich board turn pos $ direction coord pos
                )  (adjacent pos)

-- 3) Condition in @isValidMove@
isSandwich :: Map Coord Disc -> Disc -> Coord -> Direction -> Bool
isSandwich board turn curpos dir =
  let res = not . null $ filter (\coord -> Map.lookup coord board == Just turn) $ safeTail $ line curpos dir
    in res

nearestDisc :: Map Coord Disc -> Disc -> Coord -> Direction -> Coord
nearestDisc board turn pos dir =
  safeHead $ filter (\coord -> Map.lookup coord board == Just turn) $ safeTail $ line curpos dir

updateBoard :: Cord -> Disc -> Board -> Board
updateBoard pos turn board = Map.union (fromList nboard) board
  where
    nboard :: [(Coord, Disc)]
    validAdj :: [Coord]
    validAdj = validAdjacent board turn pos
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

coordPlus :: Coord -> Coord -> Coord
coordPlus (x1,y1) (x2,y2) = Coord (x1+x2, y1+y2)


  -- | returns a sequence of squares from cord in direction
line :: Cord -> Direction -> [Cord]
line pos d = l
  where
    l = filter (isJust . validate) $ iterate (coordPlus $ DirToCoord pos) pos

allDirections :: [Direction]
allDirections = (toEnum <$> [0..7::Int])::[Direction]

-- | get all valid moves
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
