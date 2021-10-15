{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module TreeSearch.Haskell.Tree.Verify

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

verifiedItem :: (Bool, t, Int) -> t
verifiedItem (_, item, _) = item
verifiedFound :: (Bool, t, Int) -> Bool
verifiedFound (found, _, _) = found
verifiedPos :: (Bool, t, Int) -> Int
verifiedPos (_, _, pos) = pos

verifySearch :: ( Show t
                )
             => (t -> t -> t)
             -> (t -> t -> Bool)
             -> (t -> t -> t -> Bool)
             -> t
             -> (t -> Bool)
             -> LgRect
             -> Array2d t
             -> Array2d (Bool,t,Int)
verifySearch combineItem
             canContain
             choose
             nullItem
             stopItem
             lgRect
             frame =
    let tree = buildTree combineItem
                         lgRect
                         frame
        (Sz2 h w) = A.size frame
        s = 1
    --  oH = 1225
    --  oW = 1390
        oH = 1280
        oW = 472
    in  tabulate_2d s s
        (\ iH iW ->
           let ix = Ix2 (iH+oH) (iW+oW)
               target = (A.!) frame ix
           in  tc ("target ix:" ++ show ix ++ " target:" ++ show target) $
               if stopItem target
               then (False,target,0)
               else findInTree (canContain target)
                               (choose target)
                               tree
                               nullItem
                               (lgRectMax lgRect)
         )
    -- in  map2d (\target ->
    --        if stopItem target
    --        then (False,target)
    --        else findInTree (canContain target)
    --                        (choose target)
    --                        tree
    --                        nullItem
    --                        (lgRectMax lgRect)
    --        ) frame
