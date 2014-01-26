module Chip8.Memory where

import Chip8.Types

import Data.Array.IO

type Memory = IOUArray Addr Word8

newMemory :: IO Memory
newMemory = newArray (minBound, maxBound) 0

getByte :: Memory -> Addr -> IO Word8
getByte = readArray

getCode :: Memory -> Addr -> IO (Word8, Word8)
getCode mem addr = do
    hi <- getByte mem addr
    lo <- getByte mem (succ addr)
    return (hi, lo)
