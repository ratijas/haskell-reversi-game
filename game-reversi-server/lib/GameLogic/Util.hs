module GameLogic.Util where

minX :: Int
minX = 0

maxX :: Int
maxX = 7

minY :: Int
minY = 0

maxY :: Int
maxY = 7

collapse :: (Maybe a, Maybe b) -> Maybe (a,b)
collapse (Just x, Just y) = Just (x, y)
collapse _ = Nothing

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

safeHead :: [a] -> a
safeHead (x:xs) = Just x
