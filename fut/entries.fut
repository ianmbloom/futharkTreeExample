import "util"
import "hoas"
import "image"
import "tree/build"
import "tree/tesselate"
import "tree/search"
import "tree/verify"

import "uvNode"
import "rectNode"

let scaler (scale:i64) (x:i64):f32 =
    f32.i64 x / f32.i64 scale

let verifyUvImage [h][w]
                  (frame:[h][w][4]f32):
                  ([][]UvmRange, [][]YXIndex, i64) =
    let lgSize = i64.max (adjustedLog h) (adjustedLog w)
    let lgRect = { lgH = lgSize
                 , lgW = lgSize
                 }
    let d = powerOf2 lgSize
    let rangeFrame = map2d pixelToUvmRange frame
    let adjFrame = reframeImage d d nullUvmRange rangeFrame
    let (itemFrame, yXFrame) = verifySearch combineUvmRange
                                            canContainUvmRange
                                            chooseUvmRange
                                            nullUvmRange
                                            stopUvmRange
                                            lgRect
                                            adjFrame
    in (itemFrame, yXFrame, d)

let copyCell [h][w]
             (y:i64)
             (x:i64)
             (frame:[h][w][4]f32)
             (output:*[h][w][4]f32):
             *[h][w][4]f32 =
    let output0 = output  with [y,x,0] = frame[y,x,0]
    let output1 = output0 with [y,x,1] = frame[y,x,1]
    let output2 = output1 with [y,x,2] = frame[y,x,2]
    let output3 = output2 with [y,x,3] = frame[y,x,3]
    in  output3

entry verifyYxUvSearch [h][w]
                       (frame:[h][w][4]f32):
                       [][][4]f32 =
    let (_itemFrame, yXFrame, d) = verifyUvImage frame
    let frameSize = d * d
    in  loop output = tabulate_2d h w (\ _row _col -> [0,0,0,0])
        for i < frameSize
        do let ix = i % d
           let iy = i / d
           let {yI=y, xI=x} = yXFrame[iy,ix]
           -- let t = itemFrame[iy,ix].rgMin
           in  copyCell y x frame output

entry verifyUvImageOut [h][w]
                       ( frame:[h][w][4]f32):
                       ( [][][4]f32
                       , [][][2]f32) =
    let (itemFrame, yXFrame, d) = verifyUvImage frame
    in  ( map2d uvmRangeToPixel itemFrame
        , map3d (scaler d) (map2d yXToArray yXFrame)
        )

entry verifyUvGradientFrame (lgHeight:i64)
                            (lgWidth:i64):
                            ( [][][4]f32
                            , [][][2]f32) =
    let frame = gradientFrame (powerOf2 lgHeight) (powerOf2 lgWidth)
    let (itemFrame, yXFrame, d) = verifyUvImage frame
    in  ( map2d uvmRangeToPixel itemFrame
        , map3d (scaler d) (map2d yXToArray yXFrame)
        )

entry testNodeBox (lgHeight:i64) (lgWidth:i64):[][4]i64 =
    let lgRect = { lgH = lgHeight
                 , lgW = lgWidth
                 }
    let frame  = map2d wrapPoint (pointFrame (powerOf2 lgHeight) (powerOf2 lgWidth))
    let tree = buildTree combineRect
                         lgRect
                         frame
    in  map rectToArray tree

entry verifyRectFrame (lgHeight:i64) (lgWidth:i64):([][][4]i64, [][][2]i64) =
    let lgRect = {lgH = lgHeight, lgW = lgWidth}
    let frame = pointFrame (powerOf2 lgHeight) (powerOf2 lgWidth)
    let rectFrame = map2d wrapPoint frame
    let (itemFrame, yXFrame) = verifySearch combineRect
                                            canContainRect
                                            chooseRect
                                            nullRect
                                            stopRect
                                            lgRect
                                            rectFrame
    in  ( map2d rectToArray itemFrame
        , map2d yXToArray yXFrame
        )

entry testTreeBox (lgHeight:i64) (lgWidth:i64):[][4]i64 =
    let lgRect = {lgH = lgHeight, lgW = lgWidth}
    let frame  = map2d wrapPoint (pointFrame (powerOf2 lgHeight) (powerOf2 lgWidth))
    let tree   = buildTree combineRect
                           lgRect
                           frame
    in  map rectToArray tree

let searchUvTree [h][w][s]
                 (frame:[h][w][4]f32)
                 (search:[s][2]f32):
                 ( [s]UvmRange
                 , [s]YXIndex
                 ) =
    let lgSize = i64.max (adjustedLog h) (adjustedLog w)
    let lgRect = { lgH = lgSize
                 , lgW = lgSize
                 }
    let d = powerOf2 lgSize
    let rangeFrame = map2d pixelToUvmRange frame
    let rangeSearch = map uvToUvmRange search
    let adjFrame = reframeImage d d nullUvmRange rangeFrame
    in  searchTree combineUvmRange
                   canContainUvmRange
                   chooseUvmRange
                   nullUvmRange
                   stopUvmRange
                   lgRect
                   adjFrame
                   rangeSearch

entry searchUvTreeImage [h][w][s]
                        (frame:[h][w][4]f32)
                        (search:[s][2]f32):
                        ([s][4]f32, [s][2]i64) =
    let (items, yXs) = searchUvTree frame search
    in  ( map uvmRangeToPixel items
        , map yXToArray  yXs
        )

entry searchUvTreeXYPresence [h][w][s]
                             (threshold:f32)
                             (frame:[h][w][4]f32)
                             (search:[s][2]f32):
                             [s][3]i64 =
     let (items, yXs) = searchUvTree frame search
     let searchUvm = map uvToUvm search
     let minItems = map (.rgMin) items
     let presence = map2 (determinePresence threshold) searchUvm minItems
     in  map2 (\{yI=y, xI=x} p -> [x, y, i64.bool p]) yXs presence
