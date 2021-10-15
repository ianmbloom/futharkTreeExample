{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module TreeSearch.Haskell.Tree.Traverse

where

import Data.List

import TreeSearch.Debug
import TreeSearch.MassivGrid

import TreeSearch.Haskell.Type
import TreeSearch.Haskell.Util
import TreeSearch.Haskell.Hoas
import TreeSearch.Haskell.Tree.Tesselate

newStack :: (Show t) => t -> Int -> (Int, [t])
newStack t lg = (0,replicate (lg+1) t)

emptyStack :: (Show t) => (Int, [t]) -> Bool
emptyStack (top,_) = top == 0

popStack :: {-(Show t) =>-} (Int, [Int]) -> (Int,(Int,[Int]))
popStack (top, stack) = let stack' = replaceNth (top-1) (-1) stack
                        in  (stack !! (top-1),(top-1,stack'))

replaceNth :: (Show t) => Int -> t -> [t] -> [t]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

pushStack :: (Show t) => t -> (Int,[t]) -> (Int,[t])
pushStack i (top,stack) =
    let stack' = replaceNth top i stack
    in  (top+1, stack')

rootStack :: (Int,[Int]) -> (Int,[Int])
rootStack (top,stack) = pushStack 0 (top, stack)

leftChild :: Int -> Int
leftChild  pos = pos * 2 + 1

rightChild :: Int -> Int
rightChild pos = pos * 2 + 2

isLeaf :: Int -> Int -> Bool
isLeaf len pos = leftChild pos >= len

traverseBody :: (Show t)
             => (t -> Bool)
             -> [t]
             -> Int
             -> (Int,[Int])
             -> (Bool, Int, (Int,[Int]))
traverseBody canContain tree pos stack =
    let left       = leftChild  pos
        right      = rightChild pos
        leftValue  = tree !! left
        rightValue = tree !! right
        goLeft     = canContain leftValue
        goRight    = canContain rightValue
    in  if goLeft
        then let stack' = if goRight
                          then pushStack right stack
                          else stack
             in (False, left, stack')
        else if goRight
             then (False, right, stack)
             else (True , pos  , stack)

traverseTree :: (Show t)
             => (t -> Bool)
             -> [t]
             -> (Int,[Int])
             -> (Bool, Int,(Int,[Int]))
traverseTree canContain
             tree
             initStack =
    --tc ("traverseTree initStack: " ++ show initStack) $
    let sz = length tree
        (initPos, stackTail) = popStack initStack
        go (found, pos, stack) =
              if not found && not (isLeaf sz pos)
              then go (traverseBody canContain tree pos stack)
              else (found, pos, stack)
    in  go (False, initPos, stackTail)

checkItem :: (Show t)
          => (t -> t -> Bool)
          -> t
          -> Int
          -> t
          -> Int
          -> (Bool, t, Int)
checkItem choose item i other o =
    let ch = choose item other
    in
    trWhen (not ch) ("checkItem") {- ch " ++ show ch ++ " item " ++ show item ++ " other " ++ show other)-} $
    if ch
    then (True,  item , i)
    else (False, other, o)

--------------------------------------------------------------------------

findBody :: (Show t)
         => (t->Bool)
         -> (t->t->Bool)
         -> [t]
         -> Bool
         -> t
         -> Int
         -> (Int,[Int])
         -> (Bool, t, Int, (Int,[Int]))
findBody canContain
         choose
         tree
         everFound
         prevItem
         prevPos
         stack =
    -- tc ("findBody everFound:" ++ show everFound ++ " prevItem:"++show prevItem ++ " stack:" ++ show stack) $
    let (found, pos, stack') =
           traverseTree canContain
                        tree
                        stack
        checkFound = isLeaf (length tree) pos || found
        (thisFound, nextItem, nextPos) =
           --tr "thisFound next" $
           if not checkFound
           then (everFound, prevItem, prevPos)
           else let item = tree !! pos
                in  checkItem choose prevItem prevPos item pos
    in  (thisFound || everFound, nextItem, nextPos, stack')

findInTree :: (Show t)
           => (t -> Bool)
           -> (t -> t -> Bool)
           -> [t]
           -> t
           -> Int
           -> (Bool,t,Int)
findInTree canContain
           choose
           tree
           nullItem
           lg =
    -- tc ("findInTree lg:" ++ show lg) $
    let initStack = rootStack (newStack (-1) (lg*2))
        (finalEverFound, finalItem, finalPos, _) =
            let go (everFound, prevItem, prevPos, stack) =
                   if not (emptyStack stack)
                   then go (findBody canContain choose tree everFound prevItem prevPos stack)
                   else (everFound, prevItem, prevPos, stack)
            in  go (False, nullItem, 0, initStack)
    in  (finalEverFound, finalItem, finalPos)
