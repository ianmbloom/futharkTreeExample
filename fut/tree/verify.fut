import "tesselate"
import "build"
import "traverse"
import "search"
import "../hoas"

let searchFound 't ( found:bool,_item:t,_pos:i64):bool = found
let searchItem  't (_found:bool, item:t,_pos:i64):t    = item
let searchPos   't (_found:bool,_item:t, pos:i64):i64  = pos

let verifySearch 't [h][w]
                 (combineItem:t->t->t)
                 (canContain:t->t->bool)
                 (choose:t->t->t->bool)
                 (nullItem:t)
                 (stopItem:t->bool)
                 (lgRect:LgRect)
                 (frame:[h][w]t):
                 ( [][]t
                 , [][]YXIndex) =
    let tree = buildTree combineItem
                         lgRect
                         frame
    let raw = map2d (\target ->
                       if   stopItem target
                       then (false,target,0)
                       else findInTree (canContain target)
                                       (choose target)
                                       tree
                                       nullItem
                                       (lgRectMax lgRect)
                       ) frame
    let items     = map2d searchItem raw
    let positions = map2d searchPos  raw
    let yXs       = map2d (posToYx (length tree) lgRect) positions
    in  (items, yXs)

    -- let s = 32
    -- -- let oH = 1105
    -- -- let oW = 1282
    -- -- let oH = 1225
    -- -- let oW = 1390
    -- let oH = 1280
    -- let oW = 472
    -- in  tabulate_2d s s
    --     (\ iH iW ->
    --        let target = frame[iH+oH, iW+oW]
    --        in  if stopItem target
    --            then (false,target)
    --            else findInTree (canContain target)
    --                            (choose target)
    --                            tree
    --                            nullItem
    --                            (lgRectMax lgRect)
    --    )
