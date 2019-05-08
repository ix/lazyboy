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
            -- set scroll values
            write (Address scx) 0
            write (Address scy) 0
            -- set background palette
            setBackgroundPalette defaultPalette
            -- perform graphics operations
            onVblank $ do
                disableLCD
                memcpy (Name smiley) (Address $ 0x9010) $ fromIntegral $ length image
                memset (Address 0x9904) (0x992F - 0x9904) 0 -- clear the background tilemap
                write (Address background1) 1 -- write the background tile data
                setLCDControl $ defaultLCDControl { lcdDisplayEnable = True, lcdBackgroundEnable = True }
            -- halt indefinitely
            freeze

image :: [Word8]
image = [0x00,0x00,0x00,0x00,0x24,0x24,0x00,0x00,0x81,0x81,0x7e,0x7e,0x00,0x00,0x00,0x00]