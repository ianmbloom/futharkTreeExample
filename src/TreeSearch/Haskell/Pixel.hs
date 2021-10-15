module TreeSearch.Haskell.Pixel

where

type Channel = Float
type Pixel = [Channel]

addPixel :: Pixel -> Pixel -> Pixel
addPixel a b = zipWith (+) a b

dividePixel :: Float -> Pixel -> Pixel
dividePixel b a = map (/b) a

pixelToRgba :: Pixel -> (Channel,Channel,Channel,Channel)
pixelToRgba p = (p !! 0,p !! 1 ,p !! 2,p !! 3)
