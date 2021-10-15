
module Futhark.Entries where
import qualified Futhark.Raw as Raw
import qualified Futhark.Context as C
import Futhark.Fut (FutT)
import qualified Futhark.Fut as Fut
import qualified Futhark.Wrap as U
import Futhark.Types
import qualified Futhark.TypeClasses as T
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Foreign as F
import Foreign.C.Types

searchUvTreeImage
  :: Monad m 
  => F32_3d c
  -> F32_2d c
  -> FutT c m (F32_2d c, I64_2d c)
searchUvTreeImage in0 in1
  =  Fut.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> F.malloc >>= \out1
  -> C.inContextWithError context (\context'
  -> Raw.entry_searchUvTreeImage context' out0 out1 in0' in1')
  >> U.peekFreeWrapIn context out0 >>= \out0'
  -> U.peekFreeWrapIn context out1 >>= \out1'
  -> return (out0', out1')

searchUvTreeXYPresence
  :: Monad m 
  => Float
  -> F32_3d c
  -> F32_2d c
  -> FutT c m (I64_2d c)
searchUvTreeXYPresence in0 in1 in2
  =  Fut.unsafeLiftFromIO $ \context
  -> T.withFO in1 $ \in1'
  -> T.withFO in2 $ \in2'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_searchUvTreeXYPresence context' out0 in0 in1' in2')
  >> U.peekFreeWrapIn context out0

testNodeBox
  :: Monad m 
  => Int64
  -> Int64
  -> FutT c m (I64_2d c)
testNodeBox in0 in1
  =  Fut.unsafeLiftFromIO $ \context
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_testNodeBox context' out0 in0 in1)
  >> U.peekFreeWrapIn context out0

testTreeBox
  :: Monad m 
  => Int64
  -> Int64
  -> FutT c m (I64_2d c)
testTreeBox in0 in1
  =  Fut.unsafeLiftFromIO $ \context
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_testTreeBox context' out0 in0 in1)
  >> U.peekFreeWrapIn context out0

verifyRectFrame
  :: Monad m 
  => Int64
  -> Int64
  -> FutT c m (I64_3d c, I64_3d c)
verifyRectFrame in0 in1
  =  Fut.unsafeLiftFromIO $ \context
  -> F.malloc >>= \out0
  -> F.malloc >>= \out1
  -> C.inContextWithError context (\context'
  -> Raw.entry_verifyRectFrame context' out0 out1 in0 in1)
  >> U.peekFreeWrapIn context out0 >>= \out0'
  -> U.peekFreeWrapIn context out1 >>= \out1'
  -> return (out0', out1')

verifyUvGradientFrame
  :: Monad m 
  => Int64
  -> Int64
  -> FutT c m (F32_3d c, F32_3d c)
verifyUvGradientFrame in0 in1
  =  Fut.unsafeLiftFromIO $ \context
  -> F.malloc >>= \out0
  -> F.malloc >>= \out1
  -> C.inContextWithError context (\context'
  -> Raw.entry_verifyUvGradientFrame context' out0 out1 in0 in1)
  >> U.peekFreeWrapIn context out0 >>= \out0'
  -> U.peekFreeWrapIn context out1 >>= \out1'
  -> return (out0', out1')

verifyUvImageOut
  :: Monad m 
  => F32_3d c
  -> FutT c m (F32_3d c, F32_3d c)
verifyUvImageOut in0
  =  Fut.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> F.malloc >>= \out0
  -> F.malloc >>= \out1
  -> C.inContextWithError context (\context'
  -> Raw.entry_verifyUvImageOut context' out0 out1 in0')
  >> U.peekFreeWrapIn context out0 >>= \out0'
  -> U.peekFreeWrapIn context out1 >>= \out1'
  -> return (out0', out1')

verifyYxUvSearch
  :: Monad m 
  => F32_3d c
  -> FutT c m (F32_3d c)
verifyYxUvSearch in0
  =  Fut.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_verifyYxUvSearch context' out0 in0')
  >> U.peekFreeWrapIn context out0
