module Chip8.Counter
       ( Counter, newCounter
       , setCounter, getCounter, decCounter
       ) where

import Chip8.Types
import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad (unless)

newtype Counter = Counter{ getVar :: MVar Word8 }

newCounter :: IO Counter
newCounter = Counter <$> newMVar 0

setCounter :: Counter -> Word8 -> IO ()
setCounter (Counter mvar) x = do
    takeMVar mvar
    putMVar mvar x

getCounter :: Counter -> IO Word8
getCounter = readMVar . getVar

decCounter :: Counter -> IO ()
decCounter counter = do
    x <- getCounter counter
    unless (x == 0) $ setCounter counter (pred x)
