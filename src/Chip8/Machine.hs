{-# LANGUAGE RecordWildCards #-}
module Chip8.Machine
       ( Machine(frameBuffer, input), newMachine
       , stepMachine
       ) where

import Chip8.Types
import Chip8.Memory
import Chip8.Registers
import Chip8.Stack
import Chip8.OpCode
import Chip8.Timer
import Chip8.Counter
import Chip8.Input
import Chip8.Video

import Control.Concurrent.MVar
import Data.IORef
import Data.Bits
import Control.Monad
import Control.Applicative
import System.Random

data Machine = Machine{ memory :: Memory
                      , pcReg, ptrReg :: IORef Addr
                      , registers :: Registers
                      , stack :: Stack
                      , frameBuffer :: FrameBuffer
                      , inputBuffer :: MVar Nibble
                      , input :: InputState
                      , timer :: Counter
                      , killTimer :: IO ()
                      , sound :: Counter
                      , killSound :: IO ()
                      , randomGen :: IORef StdGen
                      }

newMachine :: IO Machine
newMachine = do
    memory <- newMemory
    pcReg <- newIORef 0x200
    ptrReg <- newIORef 0x000
    registers <- newRegisters
    stack <- newStack
    frameBuffer <- newFrameBuffer
    inputBuffer <- newEmptyMVar
    let pushInput key = do
            tryTakeMVar inputBuffer
            maybe (return ()) (putMVar inputBuffer) key
    input <- newInputState pushInput
    timer <- newCounter
    killTimer <- startTimer freq (decCounter timer)
    sound <- newCounter
    killSound <- startTimer freq (decCounter sound)
    randomGen <- newIORef =<< newStdGen
    return Machine{..}
  where
    freq :: Int
    freq = 1 * 1000 * 1000 `div` 60 -- 60Hz

stepMachine :: Machine -> IO ()
stepMachine Machine{..} = do
    soundOn <- (== 0) <$> getCounter sound
    when soundOn $ do
        -- Play sound
        return ()

    pc <- readIORef pcReg
    writeIORef pcReg (succ pc)
    let jump = writeIORef pcReg
        skip = modifyIORef pcReg succ

    let getReg = getRegister registers
        setReg = setRegister registers

    op <- getCode memory pc
    case decode op of
        ClearScreen -> do
            clearFrameBuffer frameBuffer
        Ret -> do
            popAddr stack >>= jump
        Sys _ -> do
            error "Unimplemented: SYS"
        Jump addr -> do
            jump addr
        Call addr -> do
            pushAddr stack addr
            jump addr
        SkipEqImm regX imm skipWhen -> do
            x <- getReg regX
            when ((x == imm) == skipWhen) skip
        SkipEqReg regX regY skipWhen -> do
            x <- getReg regX
            y <- getReg regY
            when ((x == y) == skipWhen) skip
        PutImm regX imm -> do
            setReg regX imm
        AddImm regX imm -> do
            x <- getReg regX
            setReg regX (x + imm)
        Move regX regY fun -> do
            x <- getReg regX
            y <- getReg regY
            let (x', carry) = eval fun x y
            setReg regX x'
            maybe (return ()) (setReg (R 0xf)) carry
        SetPtr addr -> do
            writeIORef ptrReg addr
        JumpPlusR0 addr -> do
            x <- getReg (R 0)
            jump (addr + fromIntegral x)
        Randomize regX mask -> do
            gen <- readIORef randomGen
            let (rnd, gen') = random gen
            writeIORef randomGen gen'
            setReg regX (rnd .&. mask)
        DrawSprite regX regY height -> do
            x <- getReg regX
            y <- getReg regY
            ptr <- readIORef ptrReg
            collisions <- fmap concat $ forM ([ptr..] `zip` [0..height]) $ \(addr, row) -> do
                mask <- getByte memory addr
                forM [0..7] $ \col -> do
                    let x' = fromIntegral x + fromIntegral col
                        y' = fromIntegral y + fromIntegral row
                    if mask `testBit` col
                      then flipPixel frameBuffer (x', y')
                      else return NoCollision
            setReg (R 0xf) (if any (== Collision) collisions then 1 else 0)
        SkipKey regX skipIfPressed -> do
            isPressed <- getKeyDown input . fromIntegral =<< getReg regX
            when (isPressed == skipIfPressed) skip
        GetTimer regX -> do
            setReg regX =<< getCounter timer
        SetTimer regX -> do
            setCounter timer =<< getReg regX
        SetSound regX -> do
            setCounter sound =<< getReg regX
        AddPtr regX -> do
            x <- getReg regX
            modifyIORef ptrReg (+ fromIntegral x)
        LoadFont regX -> do
            x <- getReg regX
            error "Unimplemented: LoadFont"
        StoreBCD regX -> do
            error "Unimplemented: StoreBCD"
        StoreRegs regMax -> do
            ptr <- readIORef ptrReg
            forM_ (zip [ptr..] [minBound..regMax]) $ \(addr, reg) -> do
                setByte memory addr =<< getReg reg
        LoadRegs regMax -> do
            ptr <- readIORef ptrReg
            forM_ (zip [ptr..] [minBound..regMax]) $ \(addr, reg) -> do
                setReg reg =<< getByte memory addr

eval :: Fun -> Word8 -> Word8 -> (Word8, Maybe Word8)
eval fun = case fun of
    Id -> noCarry const
    Or -> noCarry (.|.)
    And -> noCarry (.&.)
    XOr -> noCarry xor
    Add -> carry (+) (\x y z -> z < x)
    Subtract -> carry (-) (\x y z -> z > x)
    SubtractFlip -> carry (flip (-)) (\x y z -> z > y)
    ShiftRight -> carry (\x _ -> shiftR x 1) (\x _ _ -> testBit x 0)
    ShiftLeft -> carry (\x _ -> shiftL x 1) (\x _ _ -> testBit x 7)
  where
    noCarry f x y = (f x y, Nothing)
    carry f p x y = let z = f x y in (z, Just $ if p x y z then 1 else 0)
