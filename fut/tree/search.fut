import "../util"
import "tesselate"
import "build"
import "traverse"

--------------------------------------------------------------------------

let searchFound 't (  found:bool, _item:t, _pos:i64 ):bool = found
let searchItem  't ( _found:bool,  item:t, _pos:i64 ):t    = item
let searchPos   't ( _found:bool, _item:t,  pos:i64 ):i64  = pos

let posToYx (treeSize:i64) (lgRect:LgRect) (pos:i64):YXIndex =
    let h = powerOf2 lgRect.lgH
    let w = powerOf2 lgRect.lgW
    let rectSize = h * w
    let i = pos - (treeSize - rectSize) -- remove the non-leaf portion of the position.
    in  tesselateIndex false lgRect i

let yXToArray (yX:YXIndex):[2]i64 = [yX.yI, yX.xI]

let searchTree 't [h][w][s]
               (combineItem:t->t->t)
               (canContain:t->t->bool)
               (choose:t->t->t->bool)
               (nullItem:t)
               (stopItem:t->bool)
               (lgRect:LgRect)
               (frame:[h][w]t)
               (search:[s]t):
               ( [s]t
               , [s]YXIndex
               ) =
    let tree = buildTree combineItem
                         lgRect
                         frame
    let found = map (\target ->
                    if stopItem target
                    then (false,target,0)
                    else findInTree (canContain target)
                                    (choose target)
                                    tree
                                    nullItem
                                    (lgRectMax lgRect)
                    ) search
    let items     = -- replicate s nullItem
                    map searchItem found
    let _positions = replicate s [0,0]
                    -- map searchPos found
    let yXs       = replicate s {yI=0, xI=0}
                    -- map (posToYx (length tree) lgRect) positions
    in  (items, yXs)
