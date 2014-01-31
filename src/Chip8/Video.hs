module Chip8.Video
       ( FrameBuffer
       , newFrameBuffer
       , drawFrameBuffer
       , Collision(..), combineCollisions
       , flipPixel, clearFrameBuffer
       ) where

import Chip8.Utils

import Data.Array.IO
import Data.Word
import Control.Monad (forM_, when)
import Control.Applicative
import Graphics.UI.GLUT
import Data.Sized.Unsigned
import qualified Data.Sized.Ix as Ix

newtype FrameBuffer = FrameBuffer{ getArray :: IOUArray (U6, U5) Bool }

newFrameBuffer :: IO FrameBuffer
newFrameBuffer = FrameBuffer <$> newArray (minBound, maxBound) False

data Collision = Collision
               | NoCollision
               deriving (Eq, Show)

combineCollisions :: [Collision] -> Collision
combineCollisions cs = if not (null cs) && any (== Collision) cs
                       then Collision else NoCollision

flipPixel :: FrameBuffer -> (U6, U5) -> IO Collision
flipPixel (FrameBuffer arr) pos = do
    old <- readArray arr pos
    let new = not old
    writeArray arr pos new
    postRedisplay Nothing
    return $ if old then Collision else NoCollision

clearFrameBuffer :: FrameBuffer -> IO ()
clearFrameBuffer (FrameBuffer arr) = do
    forM_ Ix.all $ \(x, y) -> do
        writeArray arr (x, y) False

drawFrameBuffer :: FrameBuffer -> DisplayCallback
drawFrameBuffer (FrameBuffer arr) = do
    viewport minBound maxBound
    preservingMatrix $ forM_ Ix.all $ \(x, y) -> do
        isOn <- readArray arr (x, y)
        when isOn $ rect2 x y
  where
    viewport :: (U6, U5) -> (U6, U5) -> DisplayCallback
    viewport (minX, minY) (maxX, maxY) = do
        scale (recip s) (negate $ recip s) 1
        translate $ Vector3 (-w / 2) (-h / 2) 0
      where
        w :: GLfloat
        w = fromIntegral maxX - fromIntegral minX + 2

        h :: GLfloat
        h = fromIntegral maxY - fromIntegral minY + 2

        s :: GLfloat
        s = min w h

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
