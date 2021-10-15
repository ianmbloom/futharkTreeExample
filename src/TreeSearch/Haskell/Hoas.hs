{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms     #-}

module TreeSearch.Haskell.Hoas

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
import qualified Data.Massiv.Array    as A

import TreeSearch.MassivGrid

import TreeSearch.Debug
import TreeSearch.Haskell.Type

tabulate :: forall t
         .  Int -> (Int -> t) -> [t]
tabulate i f = A.toList $ (A.makeArrayR A.B A.Seq (Sz1 i) f :: Array A.B Ix1 t)

tabulate_2d :: Int
            -> Int
            -> (Int -> Int -> t)
            -> Array2d t
tabulate_2d h w f =
  let f' (Ix2 h w) = f h w
  in  A.makeArrayR A.B A.Seq (Sz2 h w) f'

map2d :: (a->b)
      -> Array2d a
      -> Array2d b
map2d f = A.compute . A.map f

map2_2d :: (a -> b -> c)
        -> Array2d a
        -> Array2d b
        -> Array2d c
map2_2d f aGrid bGrid = A.compute $ A.zipWith f aGrid bGrid

tupleOp1 :: (t->t)
         -> (t,t,t,t)
         -> (t,t,t,t)
tupleOp1 f a =
    let (a0, a1, a2, a3) = a
    in  (f a0, f a1, f a2, f a3)

tupleOp2 :: (t->t->t)
         -> (t,t,t,t)
         -> (t,t,t,t)
         -> (t,t,t,t)
tupleOp2 f a b =
    let (a0, a1, a2, a3) = a
        (b0, b1, b2, b3) = b
    in  (f a0 b0, f a1 b1, f a2 b2, f a3 b3)

reduceTuple1 :: (t->t->t)
             -> (t,t,t,t)
             -> t
reduceTuple1 f (a,b,c,d) =
    f a (f b (f c d))
