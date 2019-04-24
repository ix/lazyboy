module Lazyboy.IO where

import           Control.Monad.Trans.Writer.Lazy
import           Data.Word
import           Lazyboy

-- | Loads an 8 bit immediate value into an 8 bit register
byte :: Register8 -> Word8 -> Writer [Opcode] ()
byte reg val = tell [LDrn reg val]

-- | Loads an 8 immediate value into a 16 bit memory address
write :: Word16 -> Word8 -> Writer [Opcode] ()
write addr val = tell [LDHLnn addr, LDHLn val] 

memcpy :: Word16 -> Word16 -> Writer [Opcode] ()
memcpy src dest = do
    tell []