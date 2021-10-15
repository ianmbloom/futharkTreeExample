{-# LANGUAGE PatternSynonyms #-}

module TreeSearch.Haskell.Image

where

import Data.Massiv.Array ( size
                         , pattern Sz2
                         , index'
                         , Ix2(..)
                         )
import TreeSearch.MassivGrid

import TreeSearch.Haskell.Util
import TreeSearch.Haskell.Pixel
import TreeSearch.Haskell.Hoas

reframeImage :: Int -> Int -> t -> Array2d t -> Array2d t
reframeImage h w nullItem source =
    let (Sz2 sh sw) = size source
    in  tabulate_2d h w (\ row col ->
            if row < sh && col < sw
            then index' source (Ix2 row col)
            else nullItem
            )

gradientImage :: Int -> Int -> Array2d Pixel
gradientImage h w =
    let fh = fromIntegral h
        fw = fromIntegral w
    in  tabulate_2d h w (\ row col ->
          let r = fromIntegral row / fh
              g = fromIntegral col / fw
              b = 1
              a = 1
          in  [r,g,b,a]
          )
