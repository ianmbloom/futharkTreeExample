{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TreeSearch.Haskell.Type

where

import TreeSearch.Util

import Data.Word
import Data.Int
import Data.List

import TreeSearch.Haskell.Util

newtype X = X {unX :: Int} deriving (Num, Eq, Ord)
newtype Y = Y {unY :: Int} deriving (Num, Eq, Ord)

instance Show X where
  show (X i) = show i

instance Show Y where
  show (Y i) = show i

data LgRect = LgRect
   { lgH :: Y
   , lgW :: X
   }

instance Show LgRect where
   show (LgRect lgH lgW) = "LgRect[" ++ (concat $ intersperse " "  $ map (padL 2 ' ' . show) [unY lgH, unX lgW]) ++ "]"

rectWidth :: LgRect -> X
rectWidth  (LgRect _ (X lgW)) = X $ powerOf2 lgW
rectHeight :: LgRect -> Y
rectHeight (LgRect (Y lgH) _) = Y $ powerOf2 lgH

data Point =
     Point
     { yP::Int
     , xP::Int
     }

data Rect =
     Rect
     { y0::Int
     , x0::Int
     , y1::Int
     , x1::Int
     }

instance Show Point where
  show (Point y x) = concat
                   . intersperse " "
                   . map (padL 2 ' ' . show)
                   $ [x,y]

instance Show Rect where
  show (Rect y0 x0 y1 x1) = concat
                          . intersperse " "
                          . map (padL 2 ' ' . show)
                          $ [y0,x0,y1,x1]

rects :: [Int] -> [Rect]
rects (y0:x0:y1:x1:ss) = (Rect y0 x0 y1 x1):rects ss
rects _ = []

points :: [Int] -> [Point]
points (y0:x0:ss) = (Point y0 x0):points ss
points _ = []
