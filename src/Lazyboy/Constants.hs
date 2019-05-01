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

-- | Background Palette Data
bgp :: Word16
bgp = 0xFF47

-- | LCDC Y-coordinate
ly :: Word16
ly = 0xFF44

-- | LCD Control
lcdc :: Word16
lcdc = 0xFF40

-- | Work RAM (WRAM) Bank 0
wram0 :: Word16
wram0 = 0xC000

-- | Work RAM (WRAM) Bank 1
wram1 :: Word16
wram1 = 0xD000

-- | Scroll X
scx :: Word16
scx = 0xFF42

-- | Scroll Y
scy :: Word16
scy = 0xFF43

-- | Video RAM (VRAM)
vram :: Word16
vram = 0x8000

-- | Start of 32x32 tile background map #1
background1 :: Word16
background1 = 0x9800

background2 :: Word16
background2 = 0x9C00

-- | High RAM (HRAM)
hram :: Word16
hram = 0xFF80

oam :: Word16
oam = 0xFE00