{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.Trans.Writer.Lazy
import qualified Data.Text.Lazy.IO               as T
import           Data.Word
import           Lazyboy
import           Lazyboy.Constants
import           Lazyboy.IO
import           Lazyboy.Target.ASM

main :: IO ()
main = rom >>= T.putStrLn
    where rom = compileROM $ do
            write wram0 0xEF
            write wram1 0xAB

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
