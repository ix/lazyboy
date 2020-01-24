module Main where

import qualified Data.Text.Lazy.IO  as T
import           Data.Word
import           Lazyboy
import           Lazyboy.Prelude
import           Lazyboy.Target.ASM
import           Prelude            hiding ((&&), (/=), (<), (==), (>), (||))

main :: IO ()
main = rom >>= T.putStrLn
    where rom = compileROM $ do
            byte A 0xDE
            byte B 0xDE
            if' ((A == (0xDE :: Word8)) && (A == B)) $
                write (Address wram0) 0xDE
            freeze
