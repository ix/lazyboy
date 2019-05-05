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

{-# LANGUAGE BinaryLiterals  #-}
{-# LANGUAGE RecordWildCards #-}
module Lazyboy.IO where

import           Control.Monad.Trans.RWS.Lazy
import           Data.Bits
import           Data.Word
import           Lazyboy.Control
import           Lazyboy.Types

-- | A typeclass for packing types into Word8
class Bitfield a where
    pack :: a -> Word8

-- | A type representing the monochrome shades available
data Color = White | Light | Dark | Black
    deriving (Eq, Ord)

instance Bitfield Color where
    pack White = 0b00
    pack Light = 0b01
    pack Dark  = 0b10
    pack Black = 0b11

-- | A type representing the LCD state
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


-- | A type representing the background palette
data BackgroundPalette = BackgroundPalette { bgpColor3 :: Color
                                           , bgpColor2 :: Color
                                           , bgpColor1 :: Color
                                           , bgpColor0 :: Color
                                           }

defaultPalette :: BackgroundPalette
defaultPalette = BackgroundPalette Black Dark Light White

-- | Instance of Bitfield for BackgroundPalette
instance Bitfield BackgroundPalette where
    pack BackgroundPalette {..} = zeroBits .|. zero .|. one .|. two .|. three
        where zero  = pack bgpColor0
              one   = pack bgpColor1 `shiftL` 2
              two   = pack bgpColor2 `shiftL` 4
              three = pack bgpColor3 `shiftL` 6

-- | Write a byte to a register
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

