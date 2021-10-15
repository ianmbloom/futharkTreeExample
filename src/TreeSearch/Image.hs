{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TreeSearch.Image
  ( ImageFloat(..)
  , imageSize
  , readImageFloat
  , writeImageFloat
  , readImageArr3d
  , writeImageArr3d
  , imgRgbaToArr3d
  , arr3dToImgRgba
  , fromMonoImg
  )
where

import qualified Prelude as P
import Prelude (($), (/), (*), Float(..))
import Futhark
import Futhark.Types
import qualified Futhark.Entries as E
--import Futhark.Utils

import Data.Massiv.Array
import Data.Massiv.Array.IO
import qualified Graphics.ColorModel as CM
import Data.Word
import Data.Int

import TreeSearch.MassivGrid
import TreeSearch.Haskell.Type

type ImageFloat = Image S (CM.Alpha CM.RGB) Float

word8ToFloatPixel :: Pixel (CM.Alpha CM.RGB) Word8
                  -> Pixel (CM.Alpha CM.RGB) Float
word8ToFloatPixel = P.fmap (\ i -> P.fromIntegral i / 255)

floatToWord8Pixel :: Pixel (CM.Alpha CM.RGB) Float
                  -> Pixel (CM.Alpha CM.RGB) Word8
floatToWord8Pixel = P.fmap (\ i -> P.round $ i * 255)

imageSize :: ImageFloat -> (Int64, Int64)
imageSize img =
  let (Sz2 h w) = size img
  in (P.fromIntegral h, P.fromIntegral w)

readImageFloat :: P.String -> P.IO ImageFloat
readImageFloat fileName =
  do imgWord8 <- readImage fileName :: P.IO (Image S (CM.Alpha CM.RGB) Word8)
     P.return $ computeS $ map word8ToFloatPixel imgWord8

writeImageFloat :: P.String -> ImageFloat -> P.IO ()
writeImageFloat fileName img =
  let imgWord8 = computeS $ map floatToWord8Pixel img
  in  writeImage fileName (imgWord8 :: Image S (CM.Alpha CM.RGB) Word8)

readImageArr3d :: P.String -> P.IO Array3dFloat
readImageArr3d fileName =
  do img <- readImageFloat fileName
     P.return $ imgRgbaToArr3d img

writeImageArr3d :: P.String -> Array3dFloat -> P.IO ()
writeImageArr3d fileName arr =
  do let img = arr3dToImgRgba arr
     writeImageFloat fileName img

imgRgbaToArr3d :: ( Load r' Ix3 e
                  , Load r Ix2 (Pixel (Alpha CM.RGB) e)
                  , Construct r' Ix3 e
                  , Manifest r Ix2 (Pixel (Alpha CM.RGB) e)
                  )
               => Array r  Ix2 (Pixel (Alpha CM.RGB) e)
               -> Array r' Ix3 e
imgRgbaToArr3d img =
  let (Sz2 imgHeight imgWidth) = size img
  in  makeArray Seq (Sz3 imgHeight imgWidth 4) $
      \ (Ix3 y x channel) ->
        let (CM.PixelRGBA r g b a) = img ! (Ix2 y x)
        in
        case channel of
          0 -> r
          1 -> g
          2 -> b
          3 -> a

arr3dToImgRgba :: ( Load r Ix2 (Pixel (Alpha CM.RGB) e)
                  , Load r' Ix3 e
                  , Construct r Ix2 (Pixel (Alpha CM.RGB) e)
                  , Manifest r' Ix3 e
                  )
               => Array r' Ix3 e
               -> Array r  Ix2 (Pixel (Alpha CM.RGB) e)
arr3dToImgRgba array =
  let (Sz3 arrayHeight arrayWidth _) = size array
  in makeArray Seq (Sz2 arrayHeight arrayWidth) $
       \ (Ix2 y x) ->
       let r = array ! (Ix3 y x 0)
           g = array ! (Ix3 y x 1)
           b = array ! (Ix3 y x 2)
           a = array ! (Ix3 y x 3)
       in  CM.PixelRGBA r g b a

fromMonoImg ::( Load r' Ix2 e
              , Manifest r' Ix2 e
              , P.Integral e
              )
              => Array r' Ix2 e
              -> ImageFloat
fromMonoImg array =
  let (Sz2 arrayHeight arrayWidth) = size array
  in makeArray Seq (Sz2 arrayHeight arrayWidth) $
       \ (Ix2 y x) ->
       let gray = P.fromIntegral $ array ! (Ix2 y x)
       in  CM.PixelRGBA gray gray gray 1
