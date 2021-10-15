{-# LANGUAGE RankNTypes, ExistentialQuantification, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Futhark.Types where
import qualified Futhark.Raw as Raw
import Futhark.Wrap
import Futhark.TypeClasses
import qualified Foreign as F
import qualified Data.Massiv.Array as M
import qualified Control.Concurrent.MVar as MV
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CBool(..), CSize(..), CChar(..), CFile(..))
import Foreign.Ptr (Ptr)
import Control.DeepSeq (rwhnf)

data F32_2d c = F32_2d (MV.MVar Int) (F.ForeignPtr Raw.Futhark_f32_2d)
instance FutharkObject F32_2d Raw.Futhark_f32_2d where
  wrapFO = F32_2d
  freeFO = Raw.free_f32_2d
  fromFO (F32_2d rc fp) = (rc, fp)
instance FutharkArray F32_2d Raw.Futhark_f32_2d M.Ix2 Float where
  shapeFA  = to2d Raw.shape_f32_2d
  newFA    = from2d Raw.new_f32_2d
  valuesFA = Raw.values_f32_2d

data F32_3d c = F32_3d (MV.MVar Int) (F.ForeignPtr Raw.Futhark_f32_3d)
instance FutharkObject F32_3d Raw.Futhark_f32_3d where
  wrapFO = F32_3d
  freeFO = Raw.free_f32_3d
  fromFO (F32_3d rc fp) = (rc, fp)
instance FutharkArray F32_3d Raw.Futhark_f32_3d M.Ix3 Float where
  shapeFA  = to3d Raw.shape_f32_3d
  newFA    = from3d Raw.new_f32_3d
  valuesFA = Raw.values_f32_3d

data I64_2d c = I64_2d (MV.MVar Int) (F.ForeignPtr Raw.Futhark_i64_2d)
instance FutharkObject I64_2d Raw.Futhark_i64_2d where
  wrapFO = I64_2d
  freeFO = Raw.free_i64_2d
  fromFO (I64_2d rc fp) = (rc, fp)
instance FutharkArray I64_2d Raw.Futhark_i64_2d M.Ix2 Int64 where
  shapeFA  = to2d Raw.shape_i64_2d
  newFA    = from2d Raw.new_i64_2d
  valuesFA = Raw.values_i64_2d

data I64_3d c = I64_3d (MV.MVar Int) (F.ForeignPtr Raw.Futhark_i64_3d)
instance FutharkObject I64_3d Raw.Futhark_i64_3d where
  wrapFO = I64_3d
  freeFO = Raw.free_i64_3d
  fromFO (I64_3d rc fp) = (rc, fp)
instance FutharkArray I64_3d Raw.Futhark_i64_3d M.Ix3 Int64 where
  shapeFA  = to3d Raw.shape_i64_3d
  newFA    = from3d Raw.new_i64_3d
  valuesFA = Raw.values_i64_3d
