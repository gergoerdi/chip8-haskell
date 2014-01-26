module Chip8.Video
       ( FrameBuffer
       , numRows, numCols
       , newFrameBuffer
       , drawFrameBuffer
       , Collision(..), flipPixel
       ) where

import Data.Array.IO as A
import Data.Word
import Control.Arrow ((***))
import Control.Monad (forM_, when)
import Graphics.UI.GLUT

type FrameBuffer = IOUArray (Word8, Word8) Bool

numCols :: Word8
numCols = 64

numRows :: Word8
numRows = 32

maxX :: Word8
maxX = numCols - 1

maxY :: Word8
maxY = numRows - 1

newFrameBuffer :: IO FrameBuffer
newFrameBuffer = newArray ((0, 0), (maxX, maxY)) False

data Collision = Collision
               | NoCollision
               deriving (Eq, Show)

flipPixel :: FrameBuffer -> (Word8, Word8) -> IO Collision
flipPixel fb pos = do
    old <- readArray fb pos
    let new = not old
    writeArray fb pos new
    return $ if old then Collision else NoCollision

drawFrameBuffer :: FrameBuffer -> DisplayCallback
drawFrameBuffer fb = do
    scale (recip $ fromIntegral numCols) (recip $ fromIntegral numRows) (1 :: GLfloat)
    preservingMatrix $ forM_ [0..maxX] $ \x -> forM_ [0..maxY] $ \y -> do
        isWhite <- readArray fb (x, y)
        when isWhite $ rect2 x y
  where
    rect2 :: Word8 -> Word8 -> DisplayCallback
    rect2 x y = do
        renderPrimitive Quads $ do
            point2 x1 y1
            point2 x2 y1
            point2 x2 y2
            point2 x1 y2
      where
        x1 = fromIntegral x - fromIntegral numCols / 2
        y1 = fromIntegral y - fromIntegral numRows / 2
        x2 = x1 + 1
        y2 = y1 + 1

    point2 :: GLfloat -> GLfloat -> DisplayCallback
    point2 x y = vertex $ Vertex2 x y

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f
