{-# LANGUAGE ScopedTypeVariables #-}

module GameLogic.Print (
  printBoard,
  ) where

import qualified Data.Map        as Map
import           Data.List       (transpose)
import           Control.Monad   (forM_)
import           GameLogic.Disc
import           GameLogic.Grid
import           GameLogic.Util

printBoard :: Board -> Disc -> IO ()
printBoard b turn = do
  putStrLn " |a b c d e f g h|"
  putStrLn "-+---------------+-"
  forM_ [minY..maxY] printLine
  putStrLn "-+---------------+-"
  putStrLn " |a b c d e f g h|"


  where
    printLine :: Int -> IO ()
    printLine y = do
      let y' = y + 1
      putStr (show y')
      putStr "|"
      let (spaces :: [String]) = replicate (maxX - minX) " "
      let (items :: [String]) = (concat $ transpose [cells y, spaces])
      forM_ items putStr
      putStr "|"
      putStr (show y')
      putStrLn ""


    cells :: Int -> [String]
    cells y =
      map (discToStr . cellAt y) [minX..maxX]

    cellAt :: Int -> Int -> Either () (Maybe Disc)
    cellAt y x =
      if (x, y) `elem` (allValidMoves b turn)
        then (Left ())
        else Right $ b `at` (x, y)

    discToStr :: Either () (Maybe Disc) -> String
    discToStr (Right (Just White)) = "O"
    discToStr (Right (Just Black)) = "X"
    discToStr (Right Nothing)      = "."
    discToStr (Left _)             = "*"
