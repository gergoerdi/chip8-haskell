module Chip8.Memory where

import Chip8.Types

import Data.Array.IO
import Control.Applicative

newtype Memory = Memory{ getArray :: IOUArray Addr Word8 }

newMemory :: IO Memory
newMemory = Memory <$> newArray (minBound, maxBound) 0

getByte :: Memory -> Addr -> IO Word8
getByte = readArray . getArray

getCode :: Memory -> Addr -> IO (Word8, Word8)
getCode mem addr = do
    hi <- getByte mem addr
    lo <- getByte mem (succ addr)
    return (hi, lo)
