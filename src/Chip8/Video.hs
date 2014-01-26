module Chip8.Video
       ( FrameBuffer
       , newFrameBuffer
       , drawFrameBuffer
       , Collision(..), flipPixel
       ) where

import Chip8.Utils

import Data.Array.IO
import Data.Word
import Control.Monad (forM_, when)
import Graphics.UI.GLUT
import Data.Sized.Unsigned
import qualified Data.Sized.Ix as Ix

type FrameBuffer = IOUArray (U6, U5) Bool

newFrameBuffer :: IO FrameBuffer
newFrameBuffer = newArray (minBound, maxBound) False

data Collision = Collision
               | NoCollision
               deriving (Eq, Show)

flipPixel :: FrameBuffer -> (U6, U5) -> IO Collision
flipPixel fb pos = do
    old <- readArray fb pos
    let new = not old
    writeArray fb pos new
    return $ if old then Collision else NoCollision

drawFrameBuffer :: FrameBuffer -> DisplayCallback
drawFrameBuffer fb = do
    viewport minBound maxBound
    preservingMatrix $ forM_ Ix.all $ \x -> forM_ Ix.all $ \y -> do
        isWhite <- readArray fb (x, y)
        when isWhite $ rect2 x y
  where
    viewport :: (U6, U5) -> (U6, U5) -> DisplayCallback
    viewport (minX, minY) (maxX, maxY) = do
        scale (recip w) (recip h) 1
        translate $ Vector3 (-w / 2) (-h / 2) 0
      where
        w :: GLfloat
        w = fromIntegral maxX - fromIntegral minX

        h :: GLfloat
        h = fromIntegral maxY - fromIntegral minY

    rect2 :: U6 -> U5 -> DisplayCallback
    rect2 x y = do
        renderPrimitive Quads $ do
            point2 x1 y1
            point2 x2 y1
            point2 x2 y2
            point2 x1 y2
      where
        x1 = fromIntegral x
        y1 = fromIntegral y
        x2 = x1 + 1
        y2 = y1 + 1

    point2 :: GLfloat -> GLfloat -> DisplayCallback
    point2 x y = vertex $ Vertex2 x y
