module Chip8.Registers
       ( Registers, newRegisters
       , getRegister, setRegister
       ) where

import Chip8.Types

import Data.Array.IO
import Control.Applicative

newtype Registers = Registers{ getArray :: IOUArray Reg Word8 }

newRegisters :: IO Registers
newRegisters = Registers <$> newArray (minBound, maxBound) 0

getRegister :: Registers -> Reg -> IO Word8
getRegister = readArray . getArray

setRegister :: Registers -> Reg -> Word8 -> IO ()
setRegister (Registers arr) r = writeArray arr r
