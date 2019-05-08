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
            write (Address scx) 0
            write (Address scy) 0
            setBackgroundPalette defaultPalette
            setLCDControl $ defaultLCDControl { lcdDisplayEnable = True, lcdBackgroundEnable = True }
            --memset (Address $ 0xC000) 0xFF 97
            memcpy (Name smiley) (Address $ 0x9000 + 16) $ fromIntegral $ length image
            tell [LDrrnn HL (Address 0x9800), LDHLn 1]
            freeze

          image :: [Word8]
          image = [0x00, 0x00, 0x00, 0x00, 0x24, 0x24, 0x00, 0x00, 0x81, 0x81, 0x7e, 0x7e, 0x00, 0x00, 0x00, 0x00]