{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}


module Main where

import Futhark
import Futhark.Types
import qualified Futhark.Entries as E

import TreeSearch.Image
import TreeSearch.Debug
import TreeSearch.MassivGrid
import TreeSearch.Haskell.Type

import Data.Maybe
import Control.Monad
import Foreign.Storable.Tuple

showTup (x,y) = show x ++ " -> " ++ show y

rectTree (LgRect lgH lgW) =
  do  tree :: Array2dInt64 <-
          runFutT $
              do tree <- E.testTreeBox (fromIntegral $ unY lgH) (fromIntegral $ unX lgW)
                 fromFuthark tree
      putStrLn $ "tree " ++ show lgH ++ " " ++ show lgW
      putStrLn $ showGrid2d tree
      putStrLn "done"
      ( output   :: Array3dInt64, outputXy :: Array3dInt64) <-
          runFutT $
              do frame <- E.verifyRectFrame (fromIntegral $ unY lgH) (fromIntegral $ unX lgW)
                 fromFutharkT2 frame
      putStrLn $ show3dInt64 output
      putStrLn $ show3dInt64 outputXy
      putStrLn "done"
      return ()

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
main = do mapM_ rectTree sampleRects
