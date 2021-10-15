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
import TreeSearch.Image
import TreeSearch.Debug
import TreeSearch.MassivGrid

import Foreign.Storable.Tuple

import Data.Maybe
import Control.Monad

verifyUvTree :: FilePath
             -> FilePath
             -> FilePath
             -> IO ()
verifyUvTree inPath outPath foundOutPath =
  do  input <- readImageArr3d inPath
      ( output   :: Array3dFloat, outputXy :: Array3dFloat) <-
          runFutT $
              do futInput  <- toFuthark input
                 futOutput <- E.verifyUvImageOut futInput
                 fromFutharkT2 futOutput
      putStrLn "Input"
      -- putStrLn $ showGrid3d input
      putStrLn "Output"
      -- putStrLn $ showGrid3d output
      writeImageArr3d outPath output
      -- writeImageFloat foundOutPath $ fromMonoImg outBool
      putStrLn "done"

verifyWithPaths :: (FilePath, String) -> IO ()
verifyWithPaths (fileRoot, suffix) =
       verifyUvTree ("image/"++fileRoot++"."++suffix)
                    ("outputs/"++fileRoot++"Futhark."++suffix)
                    ("outputs/"++fileRoot++"FoundFuthark."++suffix)

fileRoots :: [(FilePath, String)]
fileRoots = [ ("head"               , "png")
            , ("headmini"           , "png")
            , ("headsmall"          , "png")
            , ("headtiny"           , "png")
            , ("gradient"           , "png")
            , ("gradientmini"       , "png")
            , ("gradient128"        , "png")
            , ("gradient512"        , "png")
            , ("gradientYellow"     , "png")
            , ("gradientYellowLarge", "png")
            , ("gradientYellowPost" , "png")
            ]

main :: IO ()
main = mapM_ verifyWithPaths fileRoots
