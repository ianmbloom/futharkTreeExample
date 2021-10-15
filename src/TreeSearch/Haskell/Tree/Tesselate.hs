module TreeSearch.Haskell.Tree.Tesselate

where

import TreeSearch.Debug
import TreeSearch.Haskell.Type
import TreeSearch.Haskell.Util

lgRectSide :: Bool -> LgRect -> Int
lgRectSide axis (LgRect lgH lgW) =
  case axis of
    True  -> unY lgH
    False -> unX lgW

yXSide :: Bool -> (Y,X) -> Int
yXSide axis (y,x) =
   case axis of
     True  -> unY y
     False -> unX x

lgRectMax :: LgRect -> Int
lgRectMax (LgRect lgH lgW) = max (unY lgH) (unX lgW)

lgRectSize :: LgRect -> Int
lgRectSize (LgRect lgH lgW) = (unY lgH) + (unX lgW)

lgRectSteps :: LgRect -> [Int]
lgRectSteps rect =
  let lgMx = lgRectMax rect
  in  [0..lgMx-1]

lgDiv :: Int -> Int -> Int -> Int
lgDiv axis lgOrtho lg =
  let above = max 0 (lg-(lgOrtho-axis))
      below = min lg (lgOrtho-axis)
  in  (2*below + axis) + above

cutOffMultiplier :: Int -> Int -> Int
cutOffMultiplier lgOrtho lg =
  if lg>=lgOrtho
  then 0
  else 1

tesselate :: Int -> Int -> Int -> Int -> Int
tesselate cutOff dv lg i =
  cutOff * ((i `mod` (powerOf2 (dv+1))) `div` (powerOf2 dv)) * (powerOf2 lg)

tesselateSide :: Bool -> Bool -> LgRect -> Int -> Int -> Int
tesselateSide tesselationAxis axis lgRect lg i =
  let lgDv   = lgDiv (boolToInt tesselationAxis) (lgRectSide axis lgRect) lg
      cutOff = cutOffMultiplier (lgRectSide (not axis) lgRect) lg
  in  tesselate cutOff lgDv lg i

tesselatePointLg :: Bool -> LgRect -> Int -> Int -> (Y,X)
tesselatePointLg tesselationAxis lgRect i lg =
  ( Y $ tesselateSide (    tesselationAxis) False lgRect lg i
  , X $ tesselateSide (not tesselationAxis) True  lgRect lg i
  )

addPoint :: (Y,X) -> (Y,X) -> (Y,X)
addPoint (y0, x0) (y1, x1) = (y0 + y1, x0 + x1)

tesselateIndex :: Bool -> LgRect -> Int -> (Y,X)
tesselateIndex tesselationAxis lgRect i =
  foldl addPoint (0,0) $ map (tesselatePointLg tesselationAxis lgRect i) (lgRectSteps lgRect)

yXToIndex :: Bool -> LgRect -> (Y,X) -> Int
yXToIndex axis lgRect yX =
  let side  = lgRectSide axis lgRect
      this  = yXSide axis yX
      other = yXSide axis yX
  in  (powerOf2 side * this) + other
