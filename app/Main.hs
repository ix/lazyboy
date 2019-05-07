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
            smiley <- embedBytes image
            memcpy (Name smiley) (Address vram) $ fromIntegral $ length image
            setBackgroundPalette defaultPalette
            setLCDControl $ defaultLCDControl { lcdDisplayEnable = True, lcdBackgroundEnable = True }
            freeze

          image :: [Word8]
          image = [0x00, 0x00, 0x00, 0x00, 0x24, 0x24, 0x00, 0x00, 0x81, 0x81, 0x7e, 0x7e, 0x00, 0x00, 0x00, 0x00]


-- repeat a series of instructions n times
repeatOp :: Int -> Lazyboy () -> Lazyboy ()
repeatOp n m = when (n > 0) $ m >> repeatOp (n - 1) m
