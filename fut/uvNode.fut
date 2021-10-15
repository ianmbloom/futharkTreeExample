import "util"
import "pixel"

type Uvm =
    { uU   :Channel
    , uV   :Channel
    , uMask:Channel
    }

let uvmLowest:Uvm =
    { uU    = f32.lowest
    , uV    = f32.lowest
    , uMask = f32.lowest
    }

type UvmRange =
    { rgMin:Uvm
    , rgMax:Uvm
    }

let uvmOp2 (f:Channel->Channel->Channel) (a:Uvm) (b:Uvm):Uvm =
    { uU    = f a.uU    b.uU
    , uV    = f a.uV    b.uV
    , uMask = f a.uMask b.uMask
    }

let maxUvm (a:Uvm) (b:Uvm):Uvm = uvmOp2 f32.max a b
let minUvm (a:Uvm) (b:Uvm):Uvm = uvmOp2 f32.min a b

let uvmAnd (f:Channel->Channel->bool) (a:Uvm) (b:Uvm):bool =
    f a.uU    b.uU    &&
    f a.uV    b.uV    &&
    f a.uMask b.uMask

let gteUvm (a:Uvm) (b:Uvm):bool =
    uvmAnd (>=) a b

let ltUvm (a:Uvm) (b:Uvm):bool =
    uvmAnd (<) a b

let lteUvm (a:Uvm) (b:Uvm):bool =
    uvmAnd (<=) a b

let singletonRange (x:Uvm):UvmRange =
    { rgMin = x
    , rgMax = x
    }

let wrapNode (x:Uvm):UvmRange =
    singletonRange x

let unwrapNode (x:UvmRange):Uvm =
    x.rgMin

let combineUvmRange (a:UvmRange) (b:UvmRange):UvmRange =
    { rgMin = minUvm a.rgMin b.rgMin
    , rgMax = maxUvm a.rgMax b.rgMax
    }

let rangeInside (test:UvmRange) (container:UvmRange):bool =
    gteUvm test.rgMin container.rgMin &&
    lteUvm test.rgMax container.rgMax

let canContainUvmRange (test:UvmRange) (container:UvmRange):bool =
    rangeInside test container

let sqr (x:f32):f32 = x * x

let distUvm (a:Uvm) (b:Uvm):Channel =
     f32.abs (b.uU    - a.uU   )
   + f32.abs (b.uV    - a.uV   )
   + f32.abs (b.uMask - a.uMask)

let chooseUvmRange (target:UvmRange) (a:UvmRange) (b:UvmRange):bool =
    if a.rgMin.uMask <= 0
    then if b.rgMin.uMask <= 0
         then true  -- if both are masked take a
         else false -- otherwise take b
    else if b.rgMin.uMask <= 0
         then true
         else let distA = distUvm target.rgMin a.rgMin
              let distB = distUvm target.rgMin b.rgMin
              in  distA <= distB

let nullUvmRange:UvmRange =
    singletonRange uvmLowest

let stopUvmRange (r:UvmRange):bool =
    r.rgMin.uMask <= 0

let pixelToUvm (p:Pixel):Uvm =
    let (u, v, _norm, mask) = pixelToRgba p
    in  { uU    = u
        , uV    = v
        , uMask = mask
        }

let uvmToPixel (n:Uvm):Pixel =
   let { uU=u, uV=v, uMask=mask} = n
   in  [ u, v, 0, mask ]

let pixelToUvmRange (p:Pixel):UvmRange =
    let uvm = pixelToUvm p
    in  singletonRange uvm

let uvToUvm (uv:[2]f32):Uvm =
    {uU = uv[0], uV = uv[1], uMask = 1}

let uvToUvmRange (uv:[2]f32):UvmRange =
    singletonRange (uvToUvm uv)

let uvmRangeToPixel (n:UvmRange):Pixel =
    uvmToPixel n.rgMin

let determinePresence (threshold:f32)
                      (orig:Uvm)
                      (found:Uvm) =
    distUvm orig found <= threshold
