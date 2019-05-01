{-|
    Module      : Lazyboy.Constants
    Description : Constant definitions for Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    This module provides definitions of constants referring to the Game Boy hardware.
-}

module Lazyboy.Constants where

import           Data.Word

-- | Work RAM (WRAM) Bank 0
wram0 :: Word16
wram0 = 0xC000

-- | Work RAM (WRAM) Bank 1
wram1 :: Word16
wram1 = 0xD000

-- | Player 1 Joypad
joypad :: Word16
joypad = 0xFF00

-- | LCD Control
lcdc :: Word16
lcdc = 0xFF40

-- | LCD state 
lcdstate :: Word16
lcdstate = 0xFF41

-- | Scroll X
scx :: Word16
scx = 0xFF42

-- | Scroll Y
scy :: Word16
scy = 0xFF43

-- | LCDC Y-coordinate
ly :: Word16
ly = 0xFF44

-- | LCDC Y-compare
lyc :: Word16
lyc = 0xFF45

-- | DMA start address
dma :: Word16 
dma = 0xFF46

-- | Background Palette Data
bgp :: Word16
bgp = 0xFF47

-- | Video RAM (VRAM)
vram :: Word16
vram = 0x8000

-- | Start of 32x32 tile background map #1
background1 :: Word16
background1 = 0x9800

-- | Start of 32x32 tile background map #2
background2 :: Word16
background2 = 0x9C00

-- | High RAM (HRAM)
hram :: Word16
hram = 0xFF80

-- | OAM 
oam :: Word16
oam = 0xFE00

-- | Screen width
screenWidth :: Word8
screenWidth = 160

-- | Screen height
screenHeight :: Word8
screenHeight = 144