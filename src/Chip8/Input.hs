{-# LANGUAGE RecordWildCards #-}
module Chip8.Input
       ( InputState, newInputState
       , getKeyDown
       , handleInput
       ) where

import Chip8.Types

import Graphics.UI.GLUT
import Data.Array.IO
import Data.Word
import Control.Monad (forM_, when, guard)
import Data.Sized.Unsigned
import qualified Data.Sized.Ix as Ix

data InputState = InputState{ keyStates :: IOUArray Nibble Bool
                            , keyCallback :: Nibble -> IO ()
                            }

newInputState :: (Nibble -> IO ()) -> IO InputState
newInputState keyCallback = do
    keyStates <- newArray (minBound, maxBound) False
    return InputState{..}

getKeyDown :: InputState -> Nibble -> IO Bool
getKeyDown InputState{..} key = readArray keyStates key

handleInput :: InputState -> KeyboardMouseCallback
handleInput InputState{..} (Char c) keyState mods _pos = do
    case encodeKey c mods of
        Nothing -> return ()
        Just key -> do
            writeArray keyStates key (keyState == Down)
            when (keyState == Down) $ keyCallback key
handleInput _ _ _ _ _ = return ()

encodeKey :: Char -> Modifiers -> Maybe Nibble
encodeKey c mods = do
    guard $ ctrl mods == Up && alt mods == Up
    case c of
        -- Numpad
        '1' -> return 0x1
        '2' -> return 0x2
        '3' -> return 0x3
        'q' -> return 0x4
        'w' -> return 0x5
        'e' -> return 0x6
        'a' -> return 0x7
        's' -> return 0x8
        'd' -> return 0x9
        'x' -> return 0x0

        -- Command buttons
        'z' -> return 0xa
        'c' -> return 0xb
        '4' -> return 0xc
        'r' -> return 0xd
        'f' -> return 0xe
        'v' -> return 0xf

        _ -> Nothing
