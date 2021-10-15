module TreeSearch.Haskell.RectNode

where

import TreeSearch.Debug
import TreeSearch.MassivGrid
import TreeSearch.Haskell.Type
import TreeSearch.Haskell.Hoas
import TreeSearch.Haskell.Util

wrapPoint :: Point -> Rect
wrapPoint p =
    Rect
    { y0 = yP p
    , x0 = xP p
    , y1 = yP p + 1
    , x1 = xP p + 1
    }

stripPoint :: Rect -> Point
stripPoint rect =
    Point
    { yP = y0 rect
    , xP = x0 rect
    }

pointFrame :: Int -> Int -> Array2d Point
pointFrame h w =
    tabulate_2d h w (\ row col -> Point {yP=col,xP=row})

rectToArray :: Rect -> [Int]
rectToArray r = [y0 r, x0 r, y1 r, x1 r]

combineRect :: Rect -> Rect -> Rect
combineRect a b =
    Rect
    { y0 = min (y0 a) (y0 b)
    , x0 = min (x0 a) (x0 b)
    , y1 = max (y1 a) (y1 b)
    , x1 = max (x1 a) (x1 b)
    }

canContainRect :: Rect -> Rect -> Bool
canContainRect test container =
    let p = stripPoint test
    in  yP p >= y0 container &&
        xP p >= x0 container &&
        yP p <  y1 container &&
        xP p <  x1 container

taxiDistancePoint ::Point -> Point -> Float
taxiDistancePoint p0 p1 =
    fromIntegral (abs(yP p1 - yP p0) + abs(xP p1 - xP p0))

taxiDistanceRect :: Rect -> Rect -> Float
taxiDistanceRect r0 r1 =
    taxiDistancePoint (stripPoint r0) (stripPoint r1)

pointToArray :: Point -> [Int]
pointToArray (Point y x) = [y,x]

chooseRect :: Rect -> Rect -> Rect -> Bool
chooseRect target a b =
   let aDist = taxiDistanceRect target a
       bDist = taxiDistanceRect target b
   in  aDist < bDist

highest :: Int
highest = 2^31

nullRect :: Rect
nullRect = Rect
           { y0 = highest
           , x0 = highest
           , y1 = highest
           , x1 = highest
           }

stopRect :: Rect -> Bool
stopRect r = False
