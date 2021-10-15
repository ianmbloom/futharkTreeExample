import "tesselate"

-- Reduce the size of the frame horizontally by combining adjacent
-- pairs.
let squeezeHori 't [h][w] (combine:t->t->t) (frame:[h][w]t):[][]t =
    tabulate_2d h (w/2) (\ row col ->
        combine (frame[row,  col*2   ])
                (frame[row, (col*2)+1])
    )

-- Reduce the size of the frame vertically by combining adjacent
-- pairs.
let squeezeVert 't [h][w] (combine:t->t->t) (frame:[h][w]t):[][]t =
    tabulate_2d (h/2) w (\ row col ->
        combine (frame[ row*2   , col])
                (frame[(row*2)+1, col])
    )

-- Squeeze horizontally if true, vertically if not.
let squeezeFrameOnAxis 't [h][w] (combine:t->t->t) (axis:bool) (frame:[h][w]t):[][]t =
    if axis
    then squeezeHori combine frame
    else squeezeVert combine frame

-- Reduce one side of a log2 parameterized rect.
let reduceLgRect (axis:bool) (lgRect:LgRect):LgRect =
    if axis
    then {lgH = lgRect.lgH    , lgW = lgRect.lgW - 1}
    else {lgH = lgRect.lgH - 1, lgW = lgRect.lgW    }

-- Flatten a frame into a tree using tessalation.
let flattenTree 't [h][w] (lgRect:LgRect) (tesselationAxis:bool) (frame:[h][w]t):[]t =
    tabulate (h*w) (\ i ->
        let {yI = row, xI = col} = tesselateIndex tesselationAxis lgRect i
        in  frame[row,col]
        -- old was frame[i/w, i%w]
        )

-- Concatenate a new tree onto an old tree and pass on the next squeezed frame
-- as well as the reduced rect.
let buildBody 't [h][w] (combine:t->t->t) (lg:i64) (lgRect:LgRect) (axis:bool) (frame:[h][w]t) (oldTree:[]t):
              (i64,LgRect,bool,[][]t,[]t) =
    let newTree = flattenTree        lgRect  axis frame
    let sqFrame = squeezeFrameOnAxis combine axis frame
    let sqRect  = reduceLgRect               axis lgRect
    in  (lg-1, sqRect, !axis, sqFrame, concat newTree oldTree)

let buildTree 't [h][w] (combine:t->t->t)
                        (initLgRect:LgRect)
                        (initFrame:[h][w]t):[]t =
    let (_, finalRect, finalAxis, finalFrame, finalTree) =
             loop (lg, lgRect, axis, frame, oldTree) = ( lgRectSize initLgRect
                                                       , initLgRect
                                                       , false
                                                       , initFrame
                                                       , []
                                                       )
             while lg >= 0
             do buildBody combine lg lgRect axis frame oldTree
    let lastStep = flattenTree finalRect finalAxis finalFrame
    in  concat lastStep finalTree
