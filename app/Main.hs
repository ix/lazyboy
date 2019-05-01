{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.Trans.RWS.Lazy
import qualified Data.Text.Lazy.IO               as T
import           Data.Word
import           Lazyboy
import           Lazyboy.Constants
import           Lazyboy.IO
import           Lazyboy.Target.ASM

main :: IO ()
main = rom >>= T.putStrLn
    where rom = compileROM $ do
            write wram1 0xC0
            write (wram1 + 1) 0xDE
            write (wram0 + 10) 0xFA
            write (wram0 + 11) 0xCE
            memcpy wram1 wram0 10
            memcpy (wram0 + 10) (wram1 + 10) 10
            freeze
                

-- repeat a series of instructions n times
repeatOp :: Int -> Lazyboy () -> Lazyboy ()
repeatOp n m = when (n > 0) $ m >> repeatOp (n - 1) m
