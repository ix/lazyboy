module Lazyboy where

import           Control.Monad.Trans.Writer
import           Data.Int
import           Data.Word

-- | 8 bit registers
data Register8 = A | B | C | D | E | H | L
  deriving (Read, Show, Eq)

-- | 16 bit registers
data Register16 = BC | DE | HL
  deriving (Read, Show, Eq)

-- | GB Opcodes
data Opcode =
    LDreg Register8 Register8 -- LD r8,r8
  | LDimm Register8 Word8     -- LD r8, n8
  | LD16imm Register16 Word16 -- LD r16, n16
  | LDHLreg Register8         -- LD [HL], r8
  | LDHLimm Word8             -- LD [HL], n8
  | LDregHL Register8         -- LD r8, [HL]

{-
  LD [r16],A
  LD [n16],A
  LD [$FF00+n8],A
  LD [$FF00+C],A
  LD A,[r16]
  LD A,[n16]
  LD A,[$FF00+n8]
  LD A,[$FF00+C]
  LD [HL+],A
  LD [HL-],A
  LD A,[HL+]
  LD A,[HL-]  -}
  deriving (Read, Eq)
