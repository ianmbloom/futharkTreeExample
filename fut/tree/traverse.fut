import "../util"
import "../hoas"
import "tesselate"

let newStack   't (i:t) (lg:i64):(i64,[lg]t) = (0,replicate lg i)
let emptyStack 't [lg] (top:i64,_stack:[lg]t):bool = top == 0
let popStack   't [lg] (top:i64, stack:*[lg]t):(t,(i64,[lg]t)) = (stack[top-1],(top-1,stack))
let pushStack  't [lg] (i:t) (top:i64,stack:*[lg]t):(i64,*[lg]t) =
    let stack' = stack with [top] = i
    in  (top+1, stack')
-- let rootStack [lg] (top:i64,stack:*[lg]i64):(i64,*[lg]i64) = pushStack 0 (top, stack)

let leftChild  (pos:i64):i64 = pos * 2 + 1
let rightChild (pos:i64):i64 = pos * 2 + 2
let isLeaf (len:i64) (pos:i64) = leftChild pos >= len

let traverseBody 't [size][lg] (canContain:t->bool) (tree:[size]t) (pos:i64) (stack:(i64,*[lg]i64)):
                 (bool,i64,(i64,*[lg]i64)) =
    let left       = leftChild  pos
    let right      = rightChild pos
    let leftValue  = tree[left ]
    let rightValue = tree[right]
    let goLeft     = canContain leftValue
    let goRight    = canContain rightValue
    in  if goLeft
        then let stack' = if goRight
                          then pushStack right stack
                          else stack
             in (false, left, stack')
        else if goRight
             then (false, right, stack)
             else (true , pos  , stack)

let traverseTree 't [size][lg]
                 (canContain:t->bool)
                 (tree:[size]t)
                 (initStack:(i64,*[lg]i64))
                 :(bool,i64,(i64,[lg]i64)) =
    let (initPos, stackTail) = popStack initStack
    in
    loop (found, pos, stack) = (false, initPos, stackTail)
    while !found && !(isLeaf size pos)
    do traverseBody canContain tree pos stack

let checkItem 't (choose:t->t->bool) (item:t) (i:i64) (other:t) (o:i64):(bool,t,i64) =
    if choose item other
    then (true,  item,  i)
    else (false, other, o)

--------------------------------------------------------------------------

let findBody 't [size][lg]
             (canContain:t->bool)
             (choose:t->t->bool)
             (tree:[size]t)
             (everFound:bool)
             (prevItem:t)
             (prevPos:i64)
             (stack:(i64,*[lg]i64)):
             (bool, t, i64, (i64,[lg]i64)) =
    let (found, pos, stack') =
           traverseTree canContain
                        tree
                        stack
    let checkFound = isLeaf size pos || found
    let (thisFound, nextItem, nextPos) =
           if !checkFound
           then (everFound, prevItem, prevPos)
           else let item = tree[pos]
                in  checkItem choose prevItem prevPos item pos
    in  (thisFound || everFound, nextItem, nextPos, stack')

let findInTree 't [size]
               (canContain:t->bool)
               (choose:t->t->bool)
               (tree:[size]t)
               (nullItem:t)
               (lg:i64):
               (bool, t, i64) =
    let root = 0
    let newStack = newStack (-1) (lg*2)
    let initStack = pushStack root newStack
    let (everFound, finalItem, finalPos, _) =
        loop (everFound, prevItem, prevPos, stack) = (false, nullItem, root, initStack)
        while !(emptyStack stack)
        do findBody canContain
                    choose
                    tree
                    everFound
                    prevItem
                    prevPos
                    stack
    in  (everFound, finalItem, finalPos)
