module Control.Monad.Z80 where

import           Control.Monad.Trans.Writer
import           Data.Int
import           Data.Word

-- | 8 bit registers
data Register8 = A | B | C | D | E | H | L
  deriving (Read, Show, Eq)

-- | Z80 opcodes
data Z80 = LD Register8 Word8
  deriving (Read, Show, Eq)
