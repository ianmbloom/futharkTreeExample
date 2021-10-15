{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}

module Main where

import TreeSearch.Image
import TreeSearch.Debug
import TreeSearch.MassivGrid

import TreeSearch.Haskell.Type
import TreeSearch.Haskell.TreeTest

import Foreign.Storable.Tuple

import Data.Maybe
import Control.Monad

showTup (x,y) = show x ++ " -> " ++ show y

rectTree :: LgRect -> IO ()
rectTree (LgRect lgH lgW) =
  do  let tree :: [Rect]
          tree = testTreeBox (fromIntegral $ unY lgH) (fromIntegral $ unX lgW)
      putStrLn $ "tree " ++ show lgH ++ " " ++ show lgW
      putStrLn $ unlines $ labelRows $ map show tree
      putStrLn "done"
      let (verifiedBool, verifiedItem) =
             verifyRectFrame (fromIntegral $ unY lgH) (fromIntegral $ unX lgW)
      putStrLn $ showGrid2d verifiedBool
      putStrLn $ showGrid2d verifiedItem
      putStrLn "done"

sampleRects :: [LgRect]
sampleRects =
  [ LgRect (Y 0) (X 0)
  , LgRect (Y 1) (X 1)
  --, LgRect (Y 1) (X 0)
  --, LgRect (Y 0) (X 1)
  --, LgRect (Y 2) (X 1)
  --, LgRect (Y 1) (X 2)
  , LgRect (Y 2) (X 2)
  --, LgRect (Y 2) (X 3)
  --, LgRect (Y 3) (X 2)
  , LgRect (Y 3) (X 3)
  , LgRect (Y 4) (X 4)
  --, LgRect (Y 5) (X 0)
  --, LgRect (Y 2) (X 5)
  --, LgRect (Y 5) (X 2)
  --, LgRect (Y 0) (X 5)
  ]

main :: IO ()
main = mapM_ rectTree sampleRects
