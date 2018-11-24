{-# LANGUAGE ScopedTypeVariables #-}

module GameLogic.Print (
  printBoard,
  ) where

import qualified Data.Map        as Map
import           Data.List       (transpose)
import           Control.Monad   (forM_)
import           GameLogic.Disc
import           GameLogic.Grid

printBoard :: Board -> IO ()
printBoard (Board b) = do
  putStrLn " |a b c d e f g h|"
  putStrLn "-+---------------+-"
  forM_ [minY..maxY] printLine
  putStrLn "-+---------------+-"
  putStrLn " |a b c d e f g h|"


  where
    printLine :: Int -> IO ()
    printLine y = do
      putStr (show y)
      putStr "|"
      let (spaces :: [String]) = replicate (maxX - minX) " "
      let (items :: [String]) = (concat $ transpose [cells y, spaces])
      forM_ items putStr
      putStr "|"
      putStr (show y)
      putStrLn ""

    cells :: Int -> [String]
    cells y =
      map (discToStr . cellAt y) [minX..maxX]

    cellAt :: Int -> Int -> Maybe Disc
    cellAt y x =
      Map.lookup (x, y) b

    discToStr :: Maybe Disc -> String
    discToStr (Just White) = "O"
    discToStr (Just Black) = "X"
    discToStr Nothing      = " "
