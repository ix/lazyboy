#!/usr/bin/env stack
-- stack --resolver lts-14.21 script --package lazyboy

module Main where


import Control.Monad.Trans.Except (runExcept)
import Data.Word                  (Word8)
import Lazyboy                    
import Lazyboy.Prelude            ((&&), (==))
import Lazyboy.Target.ASM         (lazyboy)
import Prelude                    hiding ((&&), (/=), (<), (==), (>), (||))

import qualified Data.Text.IO as T

main :: IO ()
main = lazyboy $ do
  byte A 0xDE
  byte B 0xDE
  if' ((A == (0xDE :: Word8)) && (A == B)) $
    write (Address wram0) 0xDE
  freeze
