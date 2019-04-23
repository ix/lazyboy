module Lazyboy.Constants where

import Data.Word

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