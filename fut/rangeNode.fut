import "util"
import "pixel"

type Uvn =
    { uU   :Channel
    , uV   :Channel
    , uNorm:Channel
    , uMask:Channel
    }

let uvnLowest:Uvn =
    { uU    = f32.lowest
    , uV    = f32.lowest
    , uNorm = f32.lowest
    , uMask = f32.lowest
    }

type UvnRange =
    { rgMin:Uvn
    , rgMax:Uvn
    }

let uvnOp2 (f:Channel->Channel->Channel) (a:Uvn) (b:Uvn):Uvn =
    { uU    = f a.uU    b.uU
    , uV    = f a.uV    b.uV
    , uNorm = f a.uNorm b.uNorm
    , uMask = f a.uMask b.uMask
    }

let maxUvn (a:Uvn) (b:Uvn):Uvn = uvnOp2 f32.max a b
let minUvn (a:Uvn) (b:Uvn):Uvn = uvnOp2 f32.min a b

let uvnAnd (f:Channel->Channel->bool) (a:Uvn) (b:Uvn):bool =
    f a.uU    b.uU    &&
    f a.uV    b.uV    &&
    f a.uNorm b.uNorm &&
    f a.uMask b.uMask

let gteUvn (a:Uvn) (b:Uvn):bool =
    uvnAnd (>=) a b

let ltUvn (a:Uvn) (b:Uvn):bool =
    uvnAnd (<) a b

let lteUvn (a:Uvn) (b:Uvn):bool =
    uvnAnd (<=) a b

let singletonRange (x:Uvn):UvnRange =
    { rgMin = x
    , rgMax = x
    }

let wrapNode (x:Uvn):UvnRange =
    singletonRange x

let unwrapNode (x:UvnRange):Uvn =
    x.rgMin

let combineUvnRange (a:UvnRange) (b:UvnRange):UvnRange =
    { rgMin = minUvn a.rgMin b.rgMin
    , rgMax = maxUvn a.rgMax b.rgMax
    }

let rangeInside (test:UvnRange) (container:UvnRange):bool =
    gteUvn test.rgMin container.rgMin &&
    lteUvn test.rgMax container.rgMax

let canContainUvnRange (test:UvnRange) (container:UvnRange):bool =
    rangeInside test container

let sqr (x:f32):f32 = x * x

-- let distUvn (a:Uvn) (b:Uvn):Channel =
--     f32.sqrt (
--          sqr (b.uU    - a.uU   )
--        + sqr (b.uV    - a.uV   )
--          -- + sqr (b.uNorm - a.uNorm)
--     )

let distUvn (a:Uvn) (b:Uvn):Channel =
     f32.abs (b.uU    - a.uU   )
   + f32.abs (b.uV    - a.uV   )
   + f32.abs (b.uNorm - a.uNorm)
   + f32.abs (b.uMask - a.uMask)

-- let distUvn (a:Uvn) (b:Uvn):Channel =
--     f32.sqrt (
--          sqr (b.uU    - a.uU   ) +
--          sqr (b.uV    - a.uV   ) +
--          sqr (b.uNorm - a.uNorm)
--     )

let chooseUvnRange (target:UvnRange) (a:UvnRange) (b:UvnRange):bool =
    if a.rgMin.uMask <= 0
    then if b.rgMin.uMask <= 0
         then true  -- if both are masked take a
         else false -- otherwise take b
    else if b.rgMin.uMask <= 0
         then true
         else let distA = distUvn target.rgMin a.rgMin
              let distB = distUvn target.rgMin b.rgMin
              in  distA <= distB

let nullUvnRange:UvnRange =
    { rgMin = uvnLowest
    , rgMax = uvnLowest
    }

let stopUvnRange (r:UvnRange):bool =
    r.rgMin.uMask <= 0

let pixelToUvn (p:Pixel):Uvn =
    let (u, v, norm, mask) = pixelToRgba p
    in  { uU    = u
        , uV    = v
        , uNorm = norm
        , uMask = mask
        }

let pixelToNode (p:Pixel):UvnRange =
    let uvn = pixelToUvn p
    in  singletonRange uvn

let nodeToPixel (n:UvnRange):Pixel =
   let {uU=u, uV=v, uNorm=norm, uMask=mask} = n.rgMin
   in  [u, v, norm, mask]
