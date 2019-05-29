{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import qualified Data.Text.Lazy.IO  as T
import           Data.Word
import           Lazyboy
import           Lazyboy.Target.ASM

main :: IO ()
main = rom >>= T.putStrLn
    where rom = compileROM $ do
            byte A 0xDE
            byte B 0xDE
            if' ((A `equalTo` (0xDE :: Word8)) `Lazyboy.and` (A `equalTo` B)) $ do
                write (Address wram0) 0xDE
            freeze