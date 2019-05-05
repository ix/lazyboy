{-|
    Module      : Lazyboy.IO
    Description : IO library for Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    This module defines abstract IO operations for Lazyboy.
-}

{-# LANGUAGE BinaryLiterals #-}
module Lazyboy.IO where

import           Control.Monad.Trans.RWS.Lazy
import           Data.Bits
import           Data.Word
import           Lazyboy.Control
import           Lazyboy.Types

data LCDState = LCDState { lcdDisplayEnable       :: Bool
                         , lcdWindowTileMap       :: Bool
                         , lcdEnableWindowDisplay :: Bool
                         , lcdWindowSelect        :: Bool
                         , lcdTileMapSelect       :: Bool
                         , lcdObjSize             :: Bool
                         , lcdEnableObjects       :: Bool
                         , lcdBackgroundEnable    :: Bool
                         }

defaultLCDState :: LCDState
defaultLCDState = LCDState False False False False False False False False

-- | A typeclass for packing types into Word8
class Bitfield a where
    pack :: a -> Word8

-- | Instance of Bitfield for LCDState
instance Bitfield LCDState where
    pack lcds = zeroBits .|. lcdDE .|. lcdWTM .|. lcdEWD .|. lcdWS .|. lcdTMS .|. lcdOS .|. lcdEO .|. lcdBE
          where lcdDE  = if lcdDisplayEnable lcds       then 0b10000000 else 0
                lcdWTM = if lcdWindowTileMap lcds       then 0b01000000 else 0
                lcdEWD = if lcdEnableWindowDisplay lcds then 0b00100000 else 0
                lcdWS  = if lcdWindowSelect lcds        then 0b00010000 else 0
                lcdTMS = if lcdTileMapSelect lcds       then 0b00001000 else 0
                lcdOS  = if lcdObjSize lcds             then 0b00000100 else 0
                lcdEO  = if lcdEnableObjects lcds       then 0b00000010 else 0
                lcdBE  = if lcdBackgroundEnable lcds    then 0b00000001 else 0 

-- | Loads an 8-bit immediate value into an 8-bit register
byte :: Register8 -> Word8 -> Lazyboy ()
byte reg val = tell [LDrn reg val]

-- | Loads an 8-bit immediate value into a 16-bit memory address
write :: Word16 -> Word8 -> Lazyboy ()
write addr val = tell [LDrrnn HL addr, LDHLn val]

-- | Copy a region of memory to a destination (up to 255 bytes)
memcpy :: Word16 -> Word16 -> Word8 -> Lazyboy ()
memcpy src dest len = do
    -- load the destination into DE, source into HL and length into B
    tell [LDrrnn HL src, LDrrnn DE dest, LDrn B len]
    withLocalLabel $ \label -> do
        tell [LDAHLI] -- load a byte from [HL] into A and increment
        tell [LDrrA DE, INCrr DE, DECr B, JUMPif NonZero label]

