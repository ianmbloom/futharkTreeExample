{-# LANGUAGE PatternSynonyms #-}

module TreeSearch.Haskell.Util

where

import Data.Massiv.Array ( Sz2(..)
                         , pattern Sz2
                         , size
                         )
import TreeSearch.MassivGrid
import Data.Bits

powerOf2 :: Int -> Int
powerOf2 x = 1 `shiftL` x -- 2^x

pi2 :: Float
pi2 = 2 * pi

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

adjustedLog :: Int -> Int
adjustedLog x =
    if x < 1
    then 0
    else ceiling (logBase 2 $ fromIntegral x)

adjustedSize :: Int -> Int -> Int
adjustedSize h w =
    let sz = max h w
        logSz = adjustedLog sz
        adjustedSz = 2 ^ logSz
    in  adjustedSz

adjustedImageSize :: Array2d t -> Int
adjustedImageSize frame =
   let (Sz2 h w) = size frame
   in  adjustedSize h w
