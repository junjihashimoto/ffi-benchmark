module CVector where

import Foreign
import Foreign.C.Types
data Vector

foreign import ccall unsafe "vector.h &delete_vector"
  c_delete_vector  :: FunPtr( Ptr Vector -> IO () )

foreign import ccall unsafe "vector.h delete_vector"
  c_delete_vector'  :: Ptr Vector -> IO ()

foreign import ccall unsafe "vector.h new_vector"
  c_new_vector  :: CInt -> IO (Ptr Vector)

