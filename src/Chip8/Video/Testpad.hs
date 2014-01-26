import Chip8.Video
import Graphics.UI.GLUT as GLUT
import Control.Monad (forM_)

main = do
    fb <- newFrameBuffer
    forM_ [minBound..maxBound] $ \x -> do
        flipPixel fb (x, minBound)
        flipPixel fb (x, maxBound)
    forM_ [minBound..maxBound] $ \y -> do
        flipPixel fb (minBound, y)
        flipPixel fb (maxBound, y)

    (_progName, _args) <- GLUT.getArgsAndInitialize
    _window <- GLUT.createWindow "Hello World"
    displayCallback $= display fb
    GLUT.mainLoop

display :: FrameBuffer -> DisplayCallback
display fb = do
    GLUT.clear [ColorBuffer]
    loadIdentity
    drawFrameBuffer fb
    flush
