{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module TreeSearch.Haskell.Tree.Build

where

import Data.Massiv.Array ( Array
                         , S
                         , Ix1(..)
                         , Ix2(..)
                         , Sz1(..)
                         , Sz2(..)
                         , pattern Sz1
                         , pattern Sz2
                         )
import qualified Data.Massiv.Array as A
import Data.List

import TreeSearch.Debug
import TreeSearch.MassivGrid

import TreeSearch.Haskell.Type
import TreeSearch.Haskell.Hoas
import TreeSearch.Haskell.Tree.Tesselate

squeezeHori :: (Show t)
            => (t->t->t)
            -> Array2d t
            -> Array2d t
squeezeHori combine frame =
    let (Sz2 h w) = A.size frame
    in  tabulate_2d h (w `div` 2) (\ row col ->
            combine (A.index' frame (Ix2 row (col*2  )))
                    (A.index' frame (Ix2 row (col*2+1)))
        )

squeezeVert :: (Show t)
            => (t->t->t)
            -> Array2d t
            -> Array2d t
squeezeVert combine frame =
    let (Sz2 h w) = A.size frame
    in  tabulate_2d (h `div` 2) w (\ row col ->
            combine (A.index' frame (Ix2 (row*2  ) col))
                    (A.index' frame (Ix2 (row*2+1) col))
        )

squeezeFrameOnAxis :: (Show t)
                   => (t->t->t)
                   -> Bool
                   -> Array2d t
                   -> Array2d t
squeezeFrameOnAxis combine axis frame =
  if axis
  then squeezeHori combine frame
  else squeezeVert combine frame

reduceLgRect :: Bool -> LgRect -> LgRect
reduceLgRect axis lgRect =
    if axis
    then LgRect (lgH lgRect    ) (lgW lgRect - 1)
    else LgRect (lgH lgRect - 1) (lgW lgRect    )

flattenTree :: (Show t)
            => LgRect
            -> Bool
            -> Array2d t
            -> [t]
flattenTree lgRect tesselationAxis frame =
    let (Sz2 h w) = A.size frame
    in  tabulate (h*w) (\ i ->
            let (row, col) = tesselateIndex tesselationAxis lgRect i
            in  A.index' frame (Ix2 (unY row) (unX col))
            -- old was frame[i/w, i%w]
            )

buildBody :: (Show t)
          => (t -> t -> t)
          -> (LgRect, Bool, Array2d t, [t])
          -> Int
          -> (LgRect, Bool, Array2d t, [t])
buildBody combine (lgRect, axis, frame, oldTree) lg =
   let newTree = flattenTree        lgRect  axis frame
       sqFrame = squeezeFrameOnAxis combine axis frame
       sqRect  = reduceLgRect               axis lgRect
   in  (sqRect, not axis, sqFrame, concat [newTree, oldTree])

buildTree :: (Show t)
          => (t->t->t)
          -> LgRect
          -> Array2d t
          -> [t]
buildTree combine initLgRect initFrame =
    let loopStart = ( initLgRect
                    , False
                    , initFrame
                    , []
                    )
        (finalRect, finalAxis, finalFrame, finalTree) =
             foldl (buildBody combine) loopStart $ reverse [0..lgRectSize initLgRect]
        lastStep = -- tr ("flattenTree" ++ show finalRect ++ ":") $
                   flattenTree
                   finalRect
                   finalAxis
                   finalFrame
    in  concat [lastStep, finalTree]
