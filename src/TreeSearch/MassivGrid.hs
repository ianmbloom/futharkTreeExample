{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE FlexibleContexts #-}

module TreeSearch.MassivGrid
  ( Array1d(..)
  , Array2d(..)
  , Array3d(..)
  , Array2dFloat(..)
  , Array3dFloat(..)
  , Array2dInt64(..)
  , Array3dInt64(..)
  , Array2dBool(..)
  , labelRows
  , showGrid2d
  , showGrid3d
  , show2dInt64
  , show3dInt64
  , show2dFloat
  , show3dFloat
  , toList2dFloat
  , toList3dFloat
  , toList2dInt64
  , toList3dInt64
  )
where

import Data.Massiv.Array ( Array
                         , S
                         , Ix1(..)
                         , Ix2(..)
                         , Ix3(..)
                         , Sz1(..)
                         , Sz2(..)
                         , pattern Sz1
                         , pattern Sz2
                         )
import qualified Data.Massiv.Array    as A
import qualified Data.Massiv.Array.IO as A
import Data.List (intersperse)
import Data.Int
import Foreign.Storable.Tuple

import TreeSearch.Util

type Array1d t = A.Array A.B A.Ix1 t
type Array2d t = A.Array A.B A.Ix2 t
type Array3d t = A.Array A.B A.Ix3 t

type Array3dFloat = Array S Ix3 Float
type Array2dFloat = Array S Ix2 Float
type Array3dInt64 = Array S Ix3 Int64
type Array2dInt64 = Array S Ix2 Int64
type Array2dBool  = Array S Ix2 Bool

vectorRepresentation :: Show a => [a] -> String
vectorRepresentation row = "[" ++ concat (intersperse " " $ map (\y -> padL 2 ' ' (show y)) row) ++ "]"

textRepresentation :: Show a => [a] -> String
textRepresentation row = foldl (\acc y -> acc ++ (padL 4 ' ' (show y)) ++ " ") "" row

label :: Int -> String -> String
label i row = padL 3 ' ' (show i) ++ ": " ++ row

labelRows :: [String] -> [String]
labelRows = zipWith label [0..]

showGrid2d :: ( A.Source x Ix2 a
              , Show a
              )
           => Array x Ix2 a -> String
showGrid2d grid = unlines $ labelRows $ map (textRepresentation) $ A.toLists2 grid

showGrid3d :: ( A.Source x Ix3 a
              , Show a
              )
           => Array x Ix3 a -> String
showGrid3d grid = unlines $ labelRows $ map (concatMap vectorRepresentation) $ A.toLists3 grid

show3dInt64 :: Array3dInt64 -> String
show3dInt64 = showGrid3d

show2dInt64 :: Array2dInt64 -> String
show2dInt64 = showGrid2d

show3dFloat :: Array3dFloat -> String
show3dFloat = showGrid3d

show2dFloat :: Array2dFloat -> String
show2dFloat = showGrid2d

toList2dFloat :: Array2dFloat -> [[Float]]
toList2dFloat = A.toLists2

toList3dFloat :: Array3dFloat -> [[[Float]]]
toList3dFloat = A.toLists3

toList2dInt64 :: Array2dInt64 -> [[Int]]
toList2dInt64 arr = map (map fromIntegral) (A.toLists2 arr)

toList3dInt64 :: Array3dInt64 -> [[[Int]]]
toList3dInt64 arr = map (map (map fromIntegral)) (A.toLists3 arr)
