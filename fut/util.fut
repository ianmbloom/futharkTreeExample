let pi2 = 2 * f32.pi

let powerOf2 (j:i64):i64 = 1 << j -- 2^j

let adjustedLog (x:i64) : i64 =
    if x < 1
    then 0
    else i64.f32 (f32.ceil (f32.log2 (f32.i64 x)))

let adjustedSize (h:i64) (w:i64): i64 =
    let sz = i64.max h w
    let logSz = adjustedLog sz
    let adjustedSz = 2 ^ logSz
    in  adjustedSz

let adjustedImageSize 't [h][w] (_frame:[h][w]t): i64 =
   adjustedSize h w
