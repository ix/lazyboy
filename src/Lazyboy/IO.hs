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
import           Lazyboy.Constants
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
data LCDControl = LCDControl { lcdDisplayEnable       :: Bool
                         , lcdWindowTileMap       :: Bool
                         , lcdEnableWindowDisplay :: Bool
                         , lcdWindowSelect        :: Bool
                         , lcdTileMapSelect       :: Bool
                         , lcdObjSize             :: Bool
                         , lcdEnableObjects       :: Bool
                         , lcdBackgroundEnable    :: Bool
                         }

defaultLCDControl :: LCDControl
defaultLCDControl = LCDControl False False False False False False False False

-- | Instance of Bitfield for LCDControl
instance Bitfield LCDControl where
    pack lcds = zeroBits .|. lcdDE .|. lcdWTM .|. lcdEWD .|. lcdWS .|. lcdTMS .|. lcdOS .|. lcdEO .|. lcdBE
          where lcdDE  = if lcdDisplayEnable lcds       then 0b10000000 else 0
                lcdWTM = if lcdWindowTileMap lcds       then 0b01000000 else 0
                lcdEWD = if lcdEnableWindowDisplay lcds then 0b00100000 else 0
                lcdWS  = if lcdWindowSelect lcds        then 0b00010000 else 0
                lcdTMS = if lcdTileMapSelect lcds       then 0b00001000 else 0
                lcdOS  = if lcdObjSize lcds             then 0b00000100 else 0
                lcdEO  = if lcdEnableObjects lcds       then 0b00000010 else 0
                lcdBE  = if lcdBackgroundEnable lcds    then 0b00000001 else 0

setLCDControl :: LCDControl -> Lazyboy ()
setLCDControl lcd = write (Address lcdc) $ pack lcd 

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

setBackgroundPalette :: BackgroundPalette -> Lazyboy ()
setBackgroundPalette pal = write (Address bgp) $ pack pal

-- | Write a byte to a register
byte :: Register8 -> Word8 -> Lazyboy ()
byte reg val = tell [LDrn reg val]

-- | Loads an 8-bit immediate value into a 16-bit memory address
write :: Location -> Word8 -> Lazyboy ()
write addr val = tell [LDrrnn HL addr, LDHLn val]

-- | Copy a region of memory to a destination (up to 255 bytes)
memcpy :: Location -> Location -> Word8 -> Lazyboy ()
memcpy src dest len = do
    -- load the destination into DE, source into HL and length into B
    tell [LDrrnn HL src, LDrrnn DE dest, LDrn B len]
    withLocalLabel $ \label -> do
        tell [LDAHLI] -- load a byte from [HL] into A and increment
        tell [LDrrA DE, INCrr DE, DECr B, JPif NonZero (Name label)]

-- | Set a region of memory to a value (up to 255 bytes)
memset :: Location -> Word8 -> Word8 -> Lazyboy ()
memset dest len value = do
    -- load the destination into HL, length into B and value into A
    tell [LDrrnn HL dest, LDrn B len, LDrn A value]
    withLocalLabel $ \label -> do
        tell [LDHLAI] -- load A into [HL] and increment
        tell [DECr B, JPif NonZero (Name label)]