module TreeSearch.Util

where

padL :: Int -> a -> [a] -> [a]
padL s p l
    | len >= s = l
    | otherwise    = replicate (s - len) p ++ l
      where len = length l
