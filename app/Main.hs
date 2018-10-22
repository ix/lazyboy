module Main where

import           Control.Monad
import           Control.Monad.Trans.Writer.Lazy
import           Control.Monad.Z80
import           Control.Monad.Z80.ASM
import           Data.Word

main :: IO ()
main = putStrLn $ unlines $ map show $ execWriter $ do
    byte A 0xFF
    padReg <- readJoypad B
    tell [LDreg A padReg]

-- load an immediate value into a register
byte :: Register8 -> Word8 -> Writer [Z80] ()
byte reg val = tell [LDimm reg val]

-- reads the joypad state into the given register
-- returns the register written to
readJoypad :: Register8 -> Writer [Z80] Register8
readJoypad register = do
    tell [LD16imm HL 0x9400]
    tell [LDregHL register]
    return register

-- repeat a series of instructions n times
repeatZ80 :: Int -> Writer [Z80] () -> Writer [Z80] ()
repeatZ80 n m = do
    m
    when (n > 0) $ repeatZ80 (n - 1) m
