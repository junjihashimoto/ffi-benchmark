-----------------------------------------------------------------------------
-- |
-- Source      : https://github.com/Magalame/fastest-matrices
-- Copyright   : (c) 2019 Magalame
--
-- License     : BSD3
-- Maintainer  : Junji Hashimoto<junji.hashimoto@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}


module Main where

import           System.IO.Unsafe
import           CForeignPtr
import           CVector
import           Foreign
import           Control.DeepSeq

import qualified Criterion.Main as C

instance NFData (ForeignPtr a)
  where
    rnf v = v `seq` ()

instance NFData (CForeignPtr a)
  where
    rnf v = v `seq` ()

main :: IO ()
main = do 
    C.defaultMain [
        C.env (pure (3)) $ \ ~(size) ->
            C.bgroup "Vector"
            [ C.bench "new vector as ForeignPtr" $ C.nf (\s -> unsafePerformIO (c_new_vector s >>= newForeignPtr c_delete_vector )) size
            , C.bench "new vector as CForeignPtr(This is almost the same as ForeignPtr's one.)" $ C.nf (\s -> unsafePerformIO (c_new_vector s >>= newCForeignPtr c_delete_vector )) size
            , C.bench "new vector as CForeignPtr without lock" $ C.nf (\s -> unsafePerformIO (c_new_vector s >>= newCForeignPtrWithoutLock c_delete_vector )) size
            , C.bench "new vector as ForeignPtr_(ForeignPtr with cfinalizers.)" $ C.nf (\s -> unsafePerformIO (c_new_vector s >>= newForeignPtr_ )) size
            , C.bench "new vector and delete it(Raw pointer without ForeignPtr.)" $ C.nf (\s -> unsafePerformIO (c_new_vector s >>= c_delete_vector')) size
            ]
        ]

