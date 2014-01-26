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

decCounter :: Counter -> IO Bool
decCounter counter = do
    x <- getCounter counter
    if x == 0 then return False
      else do
        setCounter counter (pred x)
        return $ pred x == 0
