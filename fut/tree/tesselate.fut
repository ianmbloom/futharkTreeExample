import "../util"

type Ind = i64
type Y = Ind
type X = Ind
type Axis = bool
type Lg = Ind
type YXIndex = {yI:Y,xI:X}
type LgRect  = {lgH:Y,lgW:X}

let lgRectSide (axis:bool) (lgRect:LgRect):Lg =
  match axis
  case true  -> lgRect.lgH
  case false -> lgRect.lgW

let yXSide (axis:bool) ({yI = y, xI = x}:YXIndex):Ind =
    match axis
    case true  -> y
    case false -> x


let lgRectMax (rect:LgRect):Ind = i64.max (rect.lgH) (rect.lgW)

let lgRectSize (rect:LgRect):Ind = rect.lgH + rect.lgW

let lgDiv (axis:Axis) (lgOrtho:Lg) (lg:Lg):Ind =
  let iAxis = i64.bool axis
  let above = i64.max 0 (lg-(lgOrtho-iAxis))
  let below = i64.min lg (lgOrtho-iAxis)
  in  (2*below + iAxis) + above

let cutOffMultiplier (lgOrtho:Lg) (lg:Lg):Ind =
  if lg>=lgOrtho
  then 0
  else 1

let tesselate (cutOff:Ind) (dv:Lg) (lg:Ind) (i:Ind):Ind =
  cutOff * ((i % (powerOf2 (dv+1))) / powerOf2 dv) * (powerOf2 lg)

let tesselateSide (tesselationAxis:bool)
                  (axis:bool)
                  (lgRect:LgRect)
                  (lg:Lg)
                  (i:Ind):Ind =
  let lgDv   = lgDiv tesselationAxis (lgRectSide   axis  lgRect) lg
  let cutOff = cutOffMultiplier      (lgRectSide (!axis) lgRect) lg
  in  tesselate cutOff lgDv lg i

let tesselatePointLg (tesselationAxis:bool)
                     (lgRect:LgRect)
                     (i:Ind)
                     (lg:Lg):YXIndex =
     { yI = tesselateSide   tesselationAxis  false lgRect lg i
     , xI = tesselateSide (!tesselationAxis) true  lgRect lg i
     }

let addPoint ({yI=y0,xI=x0}:YXIndex) ({yI=y1,xI=x1}:YXIndex):YXIndex =
     { yI = y0 + y1
     , xI = x0 + x1
     }

let tesselateIndex (tesselationAxis:bool) (lgRect:LgRect) (i:Ind):YXIndex =
    let steps = lgRectMax lgRect
    in
    loop yXAcc = {yI=0,xI=0}
    for lg < steps
    do addPoint yXAcc (tesselatePointLg tesselationAxis lgRect i lg)

let yXToIndex (axis:bool) (lgRect:LgRect) (yX:YXIndex):Ind =
    let side = lgRectSide axis lgRect
    let this = yXSide axis yX
    let other = yXSide axis yX
    in  (powerOf2 side * this) + other
