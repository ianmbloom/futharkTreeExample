import "util"
import "pixel"

let reframeImage 't [sh][sw]
                (h:i64)
                (w:i64)
                (null:t)
                (source:[sh][sw]t):[h][w]t =
    tabulate_2d h w (\ row col ->
        if row < sh && col < sw
        then source[row,col]
        else null
        )

let gradientFrame (h:i64) (w:i64):[h][w]Pixel =
    let fh = f32.i64 h
    let fw = f32.i64 w
    in  tabulate_2d h w
        (\ row col ->
          let r = f32.i64 row / fh
          let g = f32.i64 col / fw
          let b = 1
          let a = 1
          in  [r,g,b,a]
        )
