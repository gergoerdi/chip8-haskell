import Chip8.Video
import Graphics.UI.GLUT as GLUT
import Control.Monad (forM_)

main = do
    fb <- newFrameBuffer
    forM_ [0..63] $ \x -> do
        flipPixel fb (x, 0)
        flipPixel fb (x, 31)
    forM_ [0..31] $ \y -> do
        flipPixel fb (0, y)
        flipPixel fb (63, y)

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
