module Chip8.Utils (both) where

import Control.Arrow ((***))

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f
