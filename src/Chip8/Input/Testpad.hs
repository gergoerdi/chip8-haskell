import Chip8.Input
import Graphics.UI.GLUT as GLUT
import Control.Monad (forM_)

main = do
    inputState <- newInputState print

    (_progName, _args) <- GLUT.getArgsAndInitialize
    _window <- GLUT.createWindow "Input test"
    displayCallback $= (GLUT.clear [ColorBuffer] >> flush)
    globalKeyRepeat $= GlobalKeyRepeatOff
    keyboardMouseCallback $= Just (handleInput inputState)
    GLUT.mainLoop
