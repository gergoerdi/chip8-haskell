{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chip8.Types (Nibble, Addr, Reg(..), Word8) where

import Data.Sized.Unsigned
import Data.Word
import Data.Ix

type Nibble = U4
type Addr = U12

newtype Reg = R U4 deriving (Show, Eq, Ord, Bounded, Enum, Ix)
