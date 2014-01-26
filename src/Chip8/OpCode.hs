module Chip8.OpCode
       ( Fun(..), Op(..)
       , decode
       ) where

import Chip8.Utils
import Chip8.Types

data Fun = Id
         | Or
         | And
         | XOr
         | Add
         | Subtract
         | ShiftRight
         | SubtractFlip
         | ShiftLeft
         deriving Show

data Op = ClearScreen
        | Ret
        | Sys Addr
        | Jump Addr
        | Call Addr
        | SkipEqImm Reg Word8 Bool -- True if skip on eq
        | SkipEqReg Reg Reg Bool
        | PutImm Reg Word8
        | AddImm Reg Word8
        | Move Reg Reg Fun
        | SetPtr Addr
        | JumpPlusR0 Addr
        | Randomize Reg Word8
        | DrawSprite Reg Reg Nibble
        | SkipKey Reg Bool -- True if skip on key pressed
        | GetTimer Reg
        | WaitKey Reg
        | SetTimer Reg
        | SetSound Reg
        | AddPtr Reg
        | LoadFont Reg
        | StoreBCD Reg
        | StoreRegs Reg
        | LoadRegs Reg
        deriving (Show)

unpack :: (Word8, Word8) -> (Nibble, Nibble, Nibble, Nibble)
unpack xy = case both unpackWord xy of
    ((h, l), (h', l')) -> (h, l, h', l')
  where
    unpackWord :: Word8 -> (Nibble, Nibble)
    unpackWord x = both fromIntegral $ x `divMod` 16

packAddr :: Nibble -> Nibble -> Nibble -> Addr
packAddr a1 a2 a3 = sum [ fromIntegral a1 * 256
                        , fromIntegral a2 * 16
                        , fromIntegral a3
                        ]

decode :: (Word8, Word8) -> Op
decode code@(hi, lo) = case codes of
    (0x0, 0x0, 0xe, 0x0) -> ClearScreen
    (0x0, 0x0, 0xe, 0xe) -> Ret
    (0x0,   _,   _,   _) -> Sys addr
    (0x1,   _,   _,   _) -> Jump addr
    (0x2,   _,   _,   _) -> Call addr
    (0x3,   x,   _,   _) -> SkipEqImm (R x) imm True
    (0x4,   x,   _,   _) -> SkipEqImm (R x) imm False
    (0x5,   x,   y, 0x0) -> SkipEqReg (R x) (R y) True
    (0x6,   x,   _,   _) -> PutImm (R x) imm
    (0x7,   x,   _,   _) -> AddImm (R x) imm
    (0x8,   x,   y, fun) -> Move (R x) (R y) (decodeFun fun)
    (0x9,   x,   y, 0x0) -> SkipEqReg (R x) (R y) False
    (0xa,   _,   _,   _) -> SetPtr addr
    (0xb,   _,   _,   _) -> JumpPlusR0 addr
    (0xc,   x,   _,   _) -> Randomize (R x) imm
    (0xd,   x,   y,   n) -> DrawSprite (R x) (R y) n
    (0xe,   x, 0x9, 0xe) -> SkipKey (R x) True
    (0xe,   x, 0xa, 0x1) -> SkipKey (R x) False
    (0xf,   x, 0x0, 0x7) -> GetTimer (R x)
    (0xf,   x, 0x0, 0xa) -> WaitKey (R x)
    (0xf,   x, 0x1, 0x5) -> SetTimer (R x)
    (0xf,   x, 0x1, 0x8) -> SetSound (R x)
    (0xf,   x, 0x1, 0xe) -> AddPtr (R x)
    (0xf,   x, 0x2, 0x9) -> LoadFont (R x)
    (0xf,   x, 0x3, 0x3) -> StoreBCD (R x)
    (0xf,   x, 0x5, 0x5) -> StoreRegs (R x)
    (0xf,   x, 0x6, 0x5) -> LoadRegs (R x)
    _                    -> fatal "Unknown opcode" code
  where
    codes@(a1, a2, a3, a4) = unpack code
    addr = packAddr a2 a3 a4
    imm = lo

    decodeFun :: Nibble -> Fun
    decodeFun 0x0 = Id
    decodeFun 0x1 = Or
    decodeFun 0x2 = And
    decodeFun 0x3 = XOr
    decodeFun 0x4 = Add
    decodeFun 0x5 = Subtract
    decodeFun 0x6 = ShiftRight
    decodeFun 0x7 = SubtractFlip
    decodeFun 0xe = ShiftLeft
    decodeFun n = fatal "Unknown Move function" n

    fatal :: (Show a) => String -> a -> b
    fatal s x = error $ s ++ ": " ++ show x
