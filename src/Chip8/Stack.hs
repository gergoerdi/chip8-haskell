{-# LANGUAGE RecordWildCards #-}
module Chip8.Stack (Stack, newStack, pushAddr, popAddr) where

import Chip8.Types

import Data.Array.IO
import Data.IORef

data Stack = Stack{ stackPtr :: IORef Nibble
                  , stackContents :: IOArray Nibble Addr
                  }

newStack :: IO Stack
newStack = do
    ptr <- newIORef 0
    storage <- newArray (minBound, maxBound) 0
    return $ Stack ptr storage

pushAddr :: Stack -> Addr -> IO ()
pushAddr Stack{..} addr = do
    ptr <- readIORef stackPtr
    writeArray stackContents ptr addr
    writeIORef stackPtr (succ ptr)

popAddr :: Stack -> IO Addr
popAddr Stack{..} = do
    ptr <- readIORef stackPtr
    addr <- readArray stackContents (pred ptr)
    writeIORef stackPtr (pred ptr)
    return addr
