module Lazyboy.IO where

import           Control.Monad.Trans.RWS.Lazy
import           Data.Word
import           Lazyboy

-- | Loads an 8-bit immediate value into an 8-bit register
byte :: Register8 -> Word8 -> Lazyboy ()
byte reg val = tell [LDrn reg val]

-- | Loads an 8-bit immediate value into a 16-bit memory address
write :: Word16 -> Word8 -> Lazyboy ()
write addr val = tell [LDrrnn HL addr, LDHLn val] 

memcpy :: Word16 -> Word16 -> Word8 -> Lazyboy ()
memcpy src dest len = do
    -- load the destination into DE, source into HL and length into B
    tell [LDrrnn HL src, LDrrnn DE dest, LDrn B len]
    withLocalLabel $ \label -> do
        tell [LDAHLI] -- load a byte from [HL] into A and increment
        tell [LDrrA DE, INCrr DE, DECr B, JUMPif NonZero label]