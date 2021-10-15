module TreeSearch.Haskell.RangeNode

where

import Data.List
import TreeSearch.Debug

import TreeSearch.Haskell.Util
import TreeSearch.Haskell.Pixel

data Uvn =
    Uvn
    { uU    :: Channel
    , uV    :: Channel
    , uNorm :: Channel
    , uMask :: Channel
    }

instance Show Uvn where
  show x = concat $ intersperse " " $ map (showFl' 3) [uU x, uV x, uNorm x, uMask x]

lowest :: Channel
lowest = -(2^32)

uvnLowest :: Uvn
uvnLowest = Uvn
    { uU    = lowest
    , uV    = lowest
    , uNorm = lowest
    , uMask = lowest
    }

data UvnRange =
    UvnRange
    { rgMin :: Uvn
    , rgMax :: Uvn
    }

instance Show UvnRange where
  show (UvnRange a b) =
    let aString = concat $ intersperse " " $ map (showFl' 3) [uU a, uV a, uNorm a, uMask a]
        bString = concat $ intersperse " " $ map (showFl' 3) [uU b, uV b, uNorm b, uMask b]
    in  "mn["++aString++"] mx["++bString++"]"

uvnOp2 :: (Channel->Channel->Channel) -> Uvn -> Uvn -> Uvn
uvnOp2 f a b = Uvn
    { uU    = f (   uU a) (   uU b)
    , uV    = f (   uV a) (   uV b)
    , uNorm = f (uNorm a) (uNorm b)
    , uMask = f (uMask a) (uMask b)
    }

maxUvn :: Uvn -> Uvn -> Uvn
maxUvn a b = uvnOp2 max a b

minUvn :: Uvn -> Uvn -> Uvn
minUvn a b = uvnOp2 min a b

uvnAnd :: (Channel -> Channel -> Bool)
       -> Uvn
       -> Uvn
       -> Bool
uvnAnd f a b =
    f (   uU a) (   uU b) &&
    f (   uV a) (   uV b) &&
    f (uNorm a) (uNorm b) &&
    f (uMask a) (uMask b)

gteUvn :: Uvn -> Uvn -> Bool
gteUvn a b = uvnAnd (>=) a b

gtUvn :: Uvn -> Uvn -> Bool
gtUvn a b = uvnAnd (>) a b

lteUvn :: Uvn -> Uvn -> Bool
lteUvn a b = uvnAnd (<=) a b

ltUvn :: Uvn -> Uvn -> Bool
ltUvn a b = uvnAnd (<) a b

singletonRange :: Uvn -> UvnRange
singletonRange x = UvnRange
    { rgMin = x
    , rgMax = x
    }

wrapNode :: Uvn -> UvnRange
wrapNode x = singletonRange x

unwrapNode :: UvnRange -> Uvn
unwrapNode x = rgMin x

combineUvnRange :: UvnRange -> UvnRange -> UvnRange
combineUvnRange a b =
    -- tr ("combineUvnRange a " ++ show a ++ " b " ++ show b ++ " -> ") $
    UvnRange
    { rgMin = minUvn (rgMin a) (rgMin b)
    , rgMax = maxUvn (rgMax a) (rgMax b)
    }

rangeInside :: UvnRange -> UvnRange -> Bool
rangeInside test container =
    gteUvn (rgMin test) (rgMin container) &&
    lteUvn (rgMax test) (rgMax container)

canContainUvnRange :: UvnRange -> UvnRange -> Bool
canContainUvnRange test container =
    -- tr ("canContainUvnRange test" ++ show test ++ " container " ++ show container) $
    rangeInside test container

distUvn :: Uvn -> Uvn -> Float
distUvn a b =
    abs (uU    b - uU    a) +
    abs (uV    b - uV    a) +
    abs (uNorm b - uNorm a) +
    abs (uMask b - uMask a)

chooseUvnRange :: UvnRange -> UvnRange -> UvnRange -> Bool
chooseUvnRange target a b =
    --trIfFalse ("chooseUvnRange target:" ++ show target ++ " a: " ++ show a ++ " b: " ++ show b) $
    if uMask (rgMin a) <= 0
    then if uMask (rgMin b) <= 0
         then True  -- if both are masked take a
         else False -- otherwise take b
    else if uMask (rgMin b) <= 0
         then True
         else let distA = distUvn (rgMin target) (rgMin a)
                  distB = distUvn (rgMin target) (rgMin b)
              in  distA <= distB

nullUvnRange :: UvnRange
nullUvnRange = UvnRange
    { rgMin = uvnLowest
    , rgMax = uvnLowest
    }

stopUvnRange :: UvnRange -> Bool
stopUvnRange r =
    uMask (rgMin r) <= 0

pixelToUvn :: Pixel -> Uvn
pixelToUvn p =
    let (u, v, norm, mask) = pixelToRgba p
    in  Uvn
        { uU    = u
        , uV    = v
        , uNorm = norm
        , uMask = mask
        }

pixelToNode :: Pixel -> UvnRange
pixelToNode p =
    let uvn = pixelToUvn p
    in  singletonRange uvn

nodeToPixel :: UvnRange -> Pixel
nodeToPixel n =
   let Uvn {uU=u, uV=v, uNorm=norm, uMask=mask} = rgMin n
   in  [u, v, norm, mask]
