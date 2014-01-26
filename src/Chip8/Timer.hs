module Chip8.Timer (Trigger, startTimer) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (when)
import Data.Function (fix)

type Trigger = IO ()

startTimer :: Int -> Trigger -> IO (IO ())
startTimer us trigger = do
    sentinel <- newMVar True
    forkIO $ fix $ \loop -> do
        threadDelay us
        trigger
        keepRunning <- readMVar sentinel
        when keepRunning loop
    return $ forceMVar sentinel False
  where
    forceMVar mv x = do
        takeMVar mv
        putMVar mv x
