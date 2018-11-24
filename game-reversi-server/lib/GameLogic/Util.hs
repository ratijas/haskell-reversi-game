module GameLogic.Util where


minX :: Int
minX = 0

minY :: Int
minY = 0




collapse :: (Maybe a, Maybe b) -> Maybe (a,b)
collapse (Just x, Just y) = Just (x, y)
collapse _ = Nothing

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

safeHead :: [a] -> a
safeHead (x:xs) = x
