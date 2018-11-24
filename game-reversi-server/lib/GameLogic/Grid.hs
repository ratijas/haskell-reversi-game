{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameLogic.Grid where
 -- (
 --  Board (..),
 --  Cord,
 --  Direction (..),
 --  allValidMoves,
 --  initBoard,

 --  -- debug
 --  adjacent,

 --  ) where

import           Control.Applicative
import           Control.Monad       (guard)
import           Debug.Trace
import           Data.List           (any, concat)
import           Data.Map            (Map, fromList, size)
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Set            as Set
import           GameLogic.Disc      (Disc (..), flip')
import           GameLogic.Util

-- | Coordinate system goes from 0 to 7
-- (x, y)
type Cord = (Int, Int)

newtype Board = Board
  { unBoard :: Map Cord Disc
  } deriving (Show, Eq)

-- | Initial position of discs on the board.
initBoard :: Int -> Board
initBoard num = Board $ Map.fromList
  [((half,       half),       White),
   ((half + 1,   half + 1),   White),
   ((half,       half + 1),   Black),
   ((half + 1,   half),       Black)]

  where half = num `div` 2
-- | Orientation of the line
-- whether it is North, south east, west, south-east, etc
-- The order is important as it matches with the adjacent square list
data Direction = NW | N | NE | E | SE | S | SW | W
  deriving (Show, Eq, Enum)

-- | From direction to relative coordinate
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
allCordinates :: Int-> [Cord]
allCordinates num = (,) <$> [minX..num] <*> [minY..num]

-- [Relative] Moore neighborhood without the origin point (0, 0).
mooreNeighborhood' :: [Cord]
mooreNeighborhood' = filter (/= (0, 0)) ((,) <$> [ -1..1 ] <*> [ -1..1 ])

-- Moore neighborhood without the origin point.
mooreNeighborhood :: Cord -> [Cord]
mooreNeighborhood pos =
  map addXY mooreNeighborhood'
  where addXY = (pos `cordPlus`)

-- Moore neighborhood without the origin point and points that lie outside of
-- the game board boundaries.
mooreNeighborhoodOnBoard :: Int-> Cord -> [Cord]
mooreNeighborhoodOnBoard num = filter (isOnBoard num). mooreNeighborhood

-- | Predicate to test whether a given coordinate lies within game board boundaries.
isOnBoard :: Int -> Cord -> Bool
isOnBoard num (x, y)
  =  x >= minX && x <= num
  && y >= minY && y <= num

-- | Cell at (x, y)
at :: Board -> Cord -> Maybe Disc
board `at` pos = Map.lookup pos $ unBoard board

endGame :: Int -> Board -> Bool
endGame num b =
  size (unBoard b) == num*num

direction
  :: Cord  -- ^ Origin point
  -> Cord  -- ^ Destination point
  -> Direction
direction (nc_x, nc_y) (oc_x, oc_y)
  | (nc_x > oc_x)  && (nc_y > oc_y) && (nc_x - oc_x == nc_y - oc_y) = NW
  | (nc_x == oc_x) && (nc_y > oc_y)  = N
  | (nc_x < oc_x)  && (nc_y > oc_y) && (oc_x - nc_x == nc_y - oc_y) = NE
  | (nc_x < oc_x)  && (nc_y == oc_y) = E
  | (nc_x < oc_x)  && (nc_y < oc_y) && (oc_x - nc_x == oc_y - nc_y)  = SE
  | (nc_x == oc_x) && (nc_y < oc_y)  = S
  | (nc_x > oc_x)  && (nc_y < oc_y) && (nc_x - oc_x == oc_y - nc_y)  = SW
  | (nc_x > oc_x)  && (nc_y == oc_y) = W
direction _ _ = error "Invalid direction"

-- | A move is valid if the cord is empty and it satisfies 2 requirements:
--   1. there is an adjacent opponent's disc
--   2. it forms a sandwich with opponent's disc inside
isValidMove :: Int -> Board -> Disc -> Cord -> Bool
isValidMove num board turn pos
  =  (isEmptyCell board pos)
  && (not . null $ discsThatWillBeFlipped num board turn pos)

-- | Is cell at given coordinate is empty?
isEmptyCell :: Board -> Cord -> Bool
isEmptyCell board pos = isNothing $ board `at` pos

-- | Is a disc at given coordinate belongs to the current player?
-- False if coordinate is empty.
isSelf :: Board -> Disc -> Cord -> Bool
isSelf board turn pos =
    (board `at` pos) == (Just turn)

-- | Is a disc at given coordinate belongs to the opponent?
-- False if coordinate is empty.
isOpponent's :: Board -> Disc -> Cord -> Bool
isOpponent's board turn pos =
    (board `at` pos) == (Just $ flip' turn)

-- | Get all cells that may be flipped if given player puts a disc at given coordinate.
-- In other words, all discs that will be flipped.
--
-- Empty result indicates that such move is invalid.
discsThatWillBeFlipped
  :: Int
  -> Board -- ^ Game board
  -> Disc  -- ^ Player who wants to put a disc here
  -> Cord  -- ^ Coordinate at which player wants to put a disc
  -> [Cord]
discsThatWillBeFlipped num board turn pos = do
  guard $ isEmptyCell board pos
  concat $ map (discsThatWillBeFlippedInDirection) directions

  where
    directions :: [Direction]
    directions  = map (direction pos) (mooreNeighborhoodOnBoard num pos)

    discsThatWillBeFlippedInDirection :: Direction -> [Cord]
    discsThatWillBeFlippedInDirection  dir = do
      let cells = drop 1 $ line num pos dir
          opp = isOpponent's board  turn
      (end :: Cord) <- take 1 $ dropWhile opp cells
      guard $ isSelf board turn end
      takeWhile opp cells

updateBoard :: Int -> Board -> Disc -> Cord -> Maybe Board
updateBoard num board turn pos = do
  coords <- case discsThatWillBeFlipped num board turn pos of
    [] -> Nothing
    xs -> Just xs
  guard $ not (null coords)
  let updates = (pos, turn) : (zip coords (repeat turn))
  -- Map.union prefers left argument when duplicate keys are encountered
  return $ Board $ Map.union (fromList updates) (unBoard board)


-- | impl for Cord
cordPlus :: Cord -> Cord -> Cord
(x1,y1) `cordPlus` (x2,y2) = (x1+x2, y1+y2)


-- | A sequence of cells from cord in direction, including origin coordinate.
line :: Int-> Cord -> Direction -> [Cord]
line num pos d = l
  where
    l = takeWhile (isOnBoard num) $ iterate (cordPlus $ dirToCord d) pos

-- | All possible directions.
allDirections :: [Direction]
allDirections = toEnum <$> [0..7::Int]

-- | Get all valid moves
allValidMoves :: Int -> Board -> Disc -> [Cord]
allValidMoves num board turn = filter is_valid empties
  where
    is_valid = isValidMove num board turn

    empties :: [Cord]
    empties = Set.toList $ Set.difference allSet filled
      where
        allSet = Set.fromList (allCordinates num)
        filled = Set.fromList (fst <$> Map.toList (unBoard board))
