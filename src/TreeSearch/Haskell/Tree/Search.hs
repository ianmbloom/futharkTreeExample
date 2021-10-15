{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module TreeSearch.Haskell.Tree.Search

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
import TreeSearch.Haskell.Util
import TreeSearch.Haskell.Hoas

import TreeSearch.Haskell.Tree.Tesselate
import TreeSearch.Haskell.Tree.Build
import TreeSearch.Haskell.Tree.Traverse

--------------------------------------------------------------------------

searchFound :: (Bool, t, Int) -> Bool
searchFound (found, _, _) = found
searchItem :: (Bool, t, Int) -> t
searchItem (_, item, _) = item
searchPos :: (Bool, t, Int) -> Int
searchPos (_, _, pos) = pos

searchTree :: ( Show t
              )
           => (t -> t -> t)
           -> (t -> t -> Bool)
           -> (t -> t -> t -> Bool)
           -> t
           -> (t -> Bool)
           -> LgRect
           -> Array2d t
           -> Array1d t
           -> Array1d (Bool,t,Int)
searchTree combineItem
           canContain
           choose
           nullItem
           stopItem
           lgRect
           frame
           search =
    let tree = buildTree combineItem
                         lgRect
                         frame
    in A.compute $
       A.map (\target ->
           if stopItem target
           then (False,target,0)
           else findInTree (canContain target)
                           (choose target)
                           tree
                           nullItem
                           (lgRectMax lgRect)
           ) search
