{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE RankNTypes #-}



module CForeignPtr where

import GHC.Ptr
import GHC.Base
import Data.IORef
-- import GHC.IORef
-- import GHC.STRef
import Unsafe.Coerce
-- import GHC.ForeignPtr


foreign import prim "myAddCFinalizzerToWeak1zh"
  myAddCFinalizzerToWeak1# :: Addr# -> Addr# -> Int# -> Addr# -> Weak# w -> State# RealWorld -> (# State# RealWorld, Int# #)

-- foreign import prim "myAddCFinalizzerToWeak2zh"
--   myAddCFinalizzerToWeak2# :: Addr# -> Addr# -> Int# -> Addr# -> Weak# w -> State# RealWorld -> (# State# RealWorld, Int# #)

-- foreign import prim "myAddCFinalizzerToWeak3zh"
--   myAddCFinalizzerToWeak3# :: Addr# -> Addr# -> Int# -> Addr# -> Weak# w -> State# RealWorld -> (# State# RealWorld, Int# #)

foreign import prim "mkWeakWithCFinalizerzh"
  mkWeakWithCFinalizer# :: Any -> Any -> Addr# -> Addr# -> Int# -> Addr# -> State# RealWorld -> (# State# RealWorld, Weak# w #)

-- foreign import prim "mkWeakWithCFinalizer2zh"
--   mkWeakWithCFinalizer2# :: Any -> Any -> Addr# -> Addr# -> Int# -> Addr# -> State# RealWorld -> (# State# RealWorld, Weak# w #)

-- foreign import prim "myMkWeakNoFinalizzerzh"
--   myMkWeakNoFinalizzer# :: Any -> Any -> State# RealWorld -> (# State# RealWorld, Weak# w #)


data MyWeak a = MyWeak (Weak# a) | Empty
data CForeignPtr a = CForeignPtr Addr# !(IORef (MyWeak ()))

newCForeignPtr :: (FunPtr (Ptr a -> IO ())) ->  Ptr a -> IO (CForeignPtr a)
newCForeignPtr (FunPtr fp) (Ptr p) = do
  ioref <- newIORef Empty
  w <- IO $ \s ->
    case mkWeakNoFinalizer# ioref () s of { (# s1, w #) ->
    case addCFinalizerToWeak# fp p 0# nullAddr# w s1 of
        (# s2, _ #) -> (# s2, MyWeak w #)
    }
  writeIORef ioref w
  return (CForeignPtr p ioref)
{-# INLINE newCForeignPtr #-}

newCForeignPtrWithMyAddCFinalizzer :: (FunPtr (Ptr a -> IO ())) ->  Ptr a -> IO (CForeignPtr a)
newCForeignPtrWithMyAddCFinalizzer (FunPtr fp) (Ptr p) = do
  ioref <- newIORef Empty
  w <- IO $ \s ->
    case mkWeakNoFinalizer# ioref () s of { (# s1, w #) ->
    case myAddCFinalizzerToWeak1# fp p 0# nullAddr# w s1 of
        (# s2, _ #) -> (# s2, MyWeak w #)
    }
  writeIORef ioref w
  return (CForeignPtr p ioref)
{-# INLINE newCForeignPtrWithMyAddCFinalizzer #-}

-- newCForeignPtr2 :: (FunPtr (Ptr a -> IO ())) ->  Ptr a -> IO (CForeignPtr a)
-- newCForeignPtr2 (FunPtr fp) (Ptr p) = do
--   ioref <- newIORef Empty
--   w <- IO $ \s ->
--     case mkWeakNoFinalizer# ioref () s of { (# s1, w #) ->
--     case myAddCFinalizzerToWeak2# fp p 0# nullAddr# w s1 of
--         (# s2, _ #) -> (# s2, MyWeak w #)
--     }
--   writeIORef ioref w
--   return (CForeignPtr p ioref)

-- newCForeignPtr3 :: (FunPtr (Ptr a -> IO ())) ->  Ptr a -> IO (CForeignPtr a)
-- newCForeignPtr3 (FunPtr fp) (Ptr p) = do
--   ioref <- newIORef Empty
--   w <- IO $ \s ->
--     case mkWeakNoFinalizer# ioref () s of { (# s1, w #) ->
--     case myAddCFinalizzerToWeak3# fp p 0# nullAddr# w s1 of
--         (# s2, _ #) -> (# s2, MyWeak w #)
--     }
--   writeIORef ioref w
--   return (CForeignPtr p ioref)

-- newCForeignPtr4 :: (FunPtr (Ptr a -> IO ())) ->  Ptr a -> IO (CForeignPtr a)
-- newCForeignPtr4 (FunPtr fp) (Ptr p) = do
--   ioref <- newIORef Empty
--   w <- IO $ \s ->
--     case mkWeakNoFinalizer# ioref () s of { (# s1, w #) -> (# s1, MyWeak w #)
-- --    case myAddCFinalizzerToWeak3# fp p 0# nullAddr# w s1 of
-- --        (# s2, _ #) -> (# s2, MyWeak w #)
--     }
--   writeIORef ioref w
--   return (CForeignPtr p ioref)

-- newCForeignPtr5 :: (FunPtr (Ptr a -> IO ())) ->  Ptr a -> IO (CForeignPtr a)
-- newCForeignPtr5 (FunPtr fp) (Ptr p) = do
--   ioref <- newIORef Empty
--   w <- IO $ \s ->
--     case mkWeakWithCFinalizer# (unsafeCoerce# ioref :: Any) (unsafeCoerce# () :: Any) fp p 0# nullAddr# s of { (# s1, w #) -> (# s1, MyWeak w #)
--     }
--   writeIORef ioref w
--   return (CForeignPtr p ioref)

newCForeignPtrWithoutLock :: (FunPtr (Ptr a -> IO ())) ->  Ptr a -> IO (CForeignPtr a)
newCForeignPtrWithoutLock (FunPtr fp) (Ptr p) = do
  ioref <- newIORef Empty
  w <- IO $ \s ->
    case mkWeakWithCFinalizer# (unsafeCoerce# ioref :: Any) (unsafeCoerce# () :: Any) fp p 0# nullAddr# s of { (# s1, w #) -> (# s1, MyWeak w #)
    }
  writeIORef ioref w
  return (CForeignPtr p ioref)
{-# INLINE newCForeignPtrWithoutLock #-}

-- newCForeignPtr6 :: (FunPtr (Ptr a -> IO ())) ->  Ptr a -> IO (CForeignPtr a)
-- newCForeignPtr6 (FunPtr fp) (Ptr p) = do
--   ioref <- newIORef Empty
--   w <- IO $ \s ->
--     case myMkWeakNoFinalizzer# (unsafeCoerce# ioref :: Any) (unsafeCoerce# () :: Any) s of { (# s1, w #) ->
--     case myAddCFinalizzerToWeak3# fp p 0# nullAddr# w s1 of
--         (# s2, _ #) -> (# s2, MyWeak w #)
--     }
--   writeIORef ioref w
--   return (CForeignPtr p ioref)

withCForeignPtr :: CForeignPtr a -> (Ptr a -> IO b) -> IO b
withCForeignPtr fo@(CForeignPtr _ r) f = IO $ \s ->
    case f (unsafeCForeignPtrToPtr fo) of
          IO action# -> keepAlive# r s action#

unsafeCForeignPtrToPtr :: CForeignPtr a -> Ptr a
unsafeCForeignPtrToPtr (CForeignPtr fo _) = Ptr fo
