module Chip8.Memory
       ( Memory, newMemory
       , getByte, setByte, getCode
       ) where

import Chip8.Types

import Data.Array.IO
import Control.Applicative

newtype Memory = Memory{ getArray :: IOUArray Addr Word8 }

newMemory :: IO Memory
newMemory = Memory <$> newArray (minBound, maxBound) 0

getByte :: Memory -> Addr -> IO Word8
getByte = readArray . getArray

setByte :: Memory -> Addr -> Word8 -> IO ()
setByte = writeArray . getArray

getCode :: Memory -> Addr -> IO (Word8, Word8)
getCode mem addr = do
    hi <- getByte mem addr
    lo <- getByte mem (succ addr)
    return (hi, lo)
