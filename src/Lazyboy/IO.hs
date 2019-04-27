module Lazyboy.IO where

import           Control.Monad.Trans.RWS.Lazy
import           Data.Word
import           Lazyboy

-- | Loads an 8 bit immediate value into an 8 bit register
byte :: Register8 -> Word8 -> Lazyboy ()
byte reg val = tell [LDrn reg val]

-- | Loads an 8 immediate value into a 16 bit memory address
write :: Word16 -> Word8 -> Lazyboy ()
write addr val = tell [LDrrnn HL addr, LDHLn val] 

memcpy :: Word16 -> Word16 -> Lazyboy ()
memcpy src dest = do
    tell []