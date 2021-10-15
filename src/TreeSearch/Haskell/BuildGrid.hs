module TreeSearch.Haskell.BuildGrid

where

import TreeSearch.Haskell.Type

import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Mutable as A
import TreeSearch.MassivGrid

import Control.Monad
import Data.Maybe

inside :: (Y,X) -> ((Y,X),Int) -> Maybe ((Y,X),Int)
inside (y1,x1) ((y,x),i) =
  if (y>=0) && (y<=y1) && (x>=0) && (x<=x1)
  then Just ((y,x),i)
  else Nothing

indexPoint :: (Y,X) -> A.Ix2
indexPoint (Y y, X x) = A.Ix2 x y

rangeSize :: (Y,X) -> A.Sz2
rangeSize (y,x) = A.Sz2 (unY y) (unX x)

writeToBlankArray2d :: (Y,X) -> [(Y,X)] -> [Int] -> IO (Array2d Int)
writeToBlankArray2d range points is =
   do let idPoints :: [((Y,X),Int)]
          idPoints = zip points is
          inPoints :: [((Y,X),Int)]
          inPoints = catMaybes . map (inside range) $ idPoints
      let fresh = A.replicate A.Seq (rangeSize range) (-1::Int) :: Array2d Int
      thawed <- A.thaw fresh
      written <- foldM (\arr (p, i)-> do A.write arr (indexPoint p) i; return arr) thawed inPoints
      frozen <- A.freeze A.Seq written
      return $  A.compute frozen
