import Chip8.Machine
import Chip8.Video
import Chip8.Input
import Chip8.Memory

import Graphics.UI.GLUT as GLUT
import Control.Monad (forever, forM_, when)
import Control.Concurrent

import qualified Data.ByteString as BS
import System.Environment

import Data.IORef
import System.IO

main = do
    hSetBuffering stdout NoBuffering

    [fileName] <- getArgs
    rom <- BS.readFile fileName

    machine <- newMachine
    forM_ ([0x200..] `zip` BS.unpack rom) $ \(addr, x) -> do
        setByte (memory machine) addr x

    (_progName, _args) <- GLUT.getArgsAndInitialize
    _window <- GLUT.createWindow "CHIP8"
    displayCallback $= display (frameBuffer machine)
    globalKeyRepeat $= GlobalKeyRepeatOff
    keyboardMouseCallback $= Just (handleInput $ input machine)

    idleCallback $= Just (stepMachine machine)
    displayCallback $= display (frameBuffer machine)

    GLUT.mainLoop

display :: FrameBuffer -> DisplayCallback
display fb = do
    GLUT.clearColor $= Color4 0.35 0.63 0.42 1
    GLUT.clear [ColorBuffer]
    loadIdentity
    drawFrameBuffer fb
    flush
