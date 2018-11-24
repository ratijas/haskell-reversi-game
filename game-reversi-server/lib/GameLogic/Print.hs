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
import           Text.Printf

letters :: String
letters = "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"

printBoard :: Int -> Board -> Disc -> IO ()
printBoard num b turn = do
  putStrLn header
  putStrLn ruler
  forM_ [minY..num] printLine
  putStrLn ruler
  putStrLn header

  where
    header = "   |" ++ (take (num * 2 + 1) letters) ++ "|"
    ruler  = "---+" ++ (concat $ take (num * 2 + 1) $ repeat "-") ++ "+---"

    printLine :: Int -> IO ()
    printLine y = do
      let y' = y + 1
      putStr (printf "% 3d" y')
      putStr "|"
      let (spaces :: [String]) = replicate (num - minX) " "
      let (items :: [String]) = (concat $ transpose [cells y, spaces])
      forM_ items putStr
      putStr "|"
      putStr (printf "% 3d" y')
      putStrLn ""


    cells :: Int -> [String]
    cells y =
      map (discToStr . cellAt y) [minX..num]

    cellAt :: Int -> Int -> Either () (Maybe Disc)
    cellAt y x =
      if (x, y) `elem` (allValidMoves num b turn)
        then (Left ())
        else Right $ b `at` (x, y)

    discToStr :: Either () (Maybe Disc) -> String
    discToStr (Right (Just White)) = "O"
    discToStr (Right (Just Black)) = "X"
    discToStr (Right Nothing)      = "."
    discToStr (Left _)             = "*"
