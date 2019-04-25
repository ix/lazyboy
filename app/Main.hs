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
            tell [LABEL ".writes"]
            write 0xC0DE 0xDD
            tell [JUMP ".writes"]

-- repeat a series of instructions n times
repeatOp :: Int -> Writer [Instruction] () -> Writer [Instruction] ()
repeatOp n m = when (n > 0) $ m >> repeatOp (n - 1) m
