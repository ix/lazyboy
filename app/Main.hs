module Main where

import           Control.Monad
import           Control.Monad.Trans.Writer.Lazy
import           Lazyboy
import           Lazyboy.Target.ASM
import           Data.Word

main :: IO ()
main = putStrLn $ unlines $ map show $ execWriter $ do
    byte A 0xFF
    padReg <- readJoypad B
    tell [LDreg A padReg]

-- load an immediate value into a register
byte :: Register8 -> Word8 -> Writer [Opcode] ()
byte reg val = tell [LDimm reg val]

-- reads the joypad state into the given register
-- returns the register written to
readJoypad :: Register8 -> Writer [Opcode] Register8
readJoypad register = do
    tell [LD16imm HL 0x9400]
    tell [LDregHL register]
    return register

-- repeat a series of instructions n times
repeatOp :: Int -> Writer [Opcode] () -> Writer [Opcode] ()
repeatOp n m = when (n > 0) $ m >> repeatOp (n - 1) m
