{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}

module Main where

import Data.Massiv.Array hiding (map, mapM_)
import qualified Graphics.ColorModel as CM

import TreeSearch.Image
import TreeSearch.Debug
import TreeSearch.MassivGrid

import TreeSearch.Haskell.Type
import TreeSearch.Haskell.Pixel
import TreeSearch.Haskell.TreeTest

import Foreign.Storable.Tuple

import Data.Maybe
import Control.Monad

imgRgbaToArrPixel :: ( Load r' Ix2 [e]
                     , Load r Ix2 (CM.Pixel (CM.Alpha CM.RGB) e)
                     , Construct r' Ix2 [e]
                     , Manifest r Ix2 (CM.Pixel (CM.Alpha CM.RGB) e)
                     )
                  => Array r  Ix2 (CM.Pixel (CM.Alpha CM.RGB) e)
                  -> Array r' Ix2 [e]
imgRgbaToArrPixel img =
  let (Sz2 imgHeight imgWidth) = size img
  in  makeArray Seq (Sz2 imgHeight imgWidth) $
      \ (Ix2 y x) ->
        let (CM.PixelRGBA r g b a) = img ! (Ix2 y x)
        in  [r, g, b, a]

arrPixelToImgRgba :: ( Load r Ix2 (CM.Pixel (CM.Alpha CM.RGB) e)
                     , Load r' Ix2 [e]
                     , Construct r Ix2 (CM.Pixel (CM.Alpha CM.RGB) e)
                     , Manifest r' Ix2 [e]
                     )
                  => Array r' Ix2 [e]
                  -> Array r  Ix2 (CM.Pixel (CM.Alpha CM.RGB) e)
arrPixelToImgRgba array =
  let (Sz2 arrayHeight arrayWidth) = size array
  in makeArray Seq (Sz2 arrayHeight arrayWidth) $
       \ (Ix2 y x) ->
       let [r,g,b,a] = array ! (Ix2 y x)
       in  CM.PixelRGBA r g b a

verifyUvTree :: FilePath
             -> FilePath
             -> FilePath
             -> IO ()
verifyUvTree inPath outPath foundOutPath =
  do  input <- imgRgbaToArrPixel <$> readImageFloat inPath
      -- putStrLn $ unlines $ labelRows $ map show $ testTreeNode input
      let (outBool :: Array2d Int, output :: Array2d Pixel) =
              verifyUvImage input
      putStrLn "Input"
      -- putStrLn $ showGrid2d input
      putStrLn "Output"
      -- putStrLn $ showGrid2d output
      writeImageFloat outPath $ arrPixelToImgRgba output
      -- writeImageFloat foundOutPath $ fromMonoImg outBool
      putStrLn "done"

verifyWithPaths :: (FilePath, String) -> IO ()
verifyWithPaths (fileRoot, suffix) =
       verifyUvTree ("image/"++fileRoot++"."++suffix)
                    ("outputs/"++fileRoot++"Haskell."++suffix)
                    ("outputs/"++fileRoot++"FoundHaskell."++suffix)

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
