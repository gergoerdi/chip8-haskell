module Chip8.Registers where

import Chip8.Types

import Data.Array.IO

type Registers = IOUArray Reg Word8

newRegisters :: IO Registers
newRegisters = newArray (minBound, maxBound) 0

getRegister :: Registers -> Reg -> IO Word8
getRegister = readArray
