{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}


module Main where

import Futhark
import Futhark.Types
import qualified Futhark.Entries as E

import TreeSearch.Haskell.Type
import TreeSearch.Haskell.Util
import TreeSearch.Haskell.Tree.Tesselate
import TreeSearch.Haskell.BuildGrid
import TreeSearch.Image
import TreeSearch.Debug
import TreeSearch.MassivGrid

import Foreign.Storable.Tuple

import Data.Maybe
import Control.Monad

showTup (x,y) = show x ++ " -> " ++ show y

rectTree (LgRect lgH lgW) =
  do  putStrLn "done"
      return ()

sampleRects :: [LgRect]
sampleRects =
  [ LgRect (Y 1) (X 1)
  , LgRect (Y 2) (X 2)
  , LgRect (Y 1) (X 0)
  , LgRect (Y 0) (X 1)
  , LgRect (Y 2) (X 1)
  , LgRect (Y 1) (X 2)
  , LgRect (Y 2) (X 2)
  , LgRect (Y 2) (X 3)
  , LgRect (Y 3) (X 2)
  , LgRect (Y 3) (X 3)
  , LgRect (Y 5) (X 0)
  , LgRect (Y 2) (X 5)
  , LgRect (Y 5) (X 2)
  , LgRect (Y 0) (X 5)
  ]

showArrGrid :: Bool -> LgRect -> IO ()
showArrGrid tesselationAxis lgRect =
    do let (LgRect lgW lgH) = lgRect
           height :: Y
           height = rectHeight lgRect
           width  :: X
           width  = rectWidth lgRect
           range :: (Y,X)
           range = (height-1, width-1)
           size :: Int
           size = powerOf2 (unY lgW + unX lgH)
           is :: [Int]
           is = [0..size-1]
           points :: [(Y,X)]
           points = map (tesselateIndex tesselationAxis lgRect) is
       arr <- writeToBlankArray2d range points is
       putStrLn $ showGrid2d arr

main :: IO ()
main = do mapM_
            (\lgRect ->
              do showArrGrid True  lgRect
                 showArrGrid False lgRect
            ) sampleRects
