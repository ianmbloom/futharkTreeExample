type Point = {yP:i64, xP:i64}
type Rect  = {y0:i64, x0:i64, y1:i64, x1:i64}

let wrapPoint (p:Point):Rect =
    { y0 = p.yP
    , x0 = p.xP
    , y1 = p.yP + 1
    , x1 = p.xP + 1
    }

let stripPoint (rect:Rect):Point =
    { yP = rect.y0
    , xP = rect.x0
    }

let pointFrame (h:i64) (w:i64):[h][w]Point =
    tabulate_2d h w (\ row col -> {yP=col,xP=row})

let rectToArray (r:Rect):[4]i64 = [r.y0,r.x0,r.y1,r.x1]

let combineRect (a:Rect) (b:Rect):Rect =
    { y0 = i64.min a.y0 b.y0
    , x0 = i64.min a.x0 b.x0
    , y1 = i64.max a.y1 b.y1
    , x1 = i64.max a.x1 b.x1
    }

let canContainRect (test:Rect) (container:Rect):bool =
    let p = stripPoint test
    in  p.yP >= container.y0 &&
        p.xP >= container.x0 &&
        p.yP <  container.y1 &&
        p.xP <  container.x1

let taxiDistancePoint (p0:Point) (p1:Point):f32 =
    f32.i64 (i64.abs(p1.yP - p0.yP) + i64.abs(p1.xP - p0.xP))

let taxiDistanceRect (r0:Rect) (r1:Rect):f32 =
    taxiDistancePoint (stripPoint r0) (stripPoint r1)

let pointToArray {yP=y, xP=x}:[2]i64 = [y,x]

let chooseRect (target:Rect) (a:Rect) (b:Rect):bool =
   let aDist = taxiDistanceRect target a
   let bDist = taxiDistanceRect target b
   in  aDist < bDist

let higher:i64 = 2^31

let nullRect = { y0 = higher
               , x0 = higher
               , y1 = higher
               , x1 = higher
               }

let stopRect (_r:Rect):bool = false
