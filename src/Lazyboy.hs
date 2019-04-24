module Lazyboy where

import           Control.Monad.Trans.Writer.Lazy
import           Data.Int
import           Data.Word

-- Rename and re-export Writer types and functions
-- type Lazyboy a = Writer [Opcode] a
-- type LazyboyT m a = WriterT [Opcode] m a

-- execLazyboy :: Lazyboy a -> [Opcode]
-- execLazyboy m = execWriter m

-- | 8 bit registers
data Register8 = A | B | C | D | E | H | L
  deriving (Read, Show, Eq)

-- | 16 bit registers
data Register16 = BC | DE | HL
  deriving (Read, Show, Eq)

-- | GB Opcodes
data Opcode =
    LDrr Register8 Register8 -- load the value in one register8 into another
  | LDrn Register8 Word8     -- load the immediate value8 into a register8
  | LDrHL Register8          -- load the value8 stored at the address in HL into a register8
  | LDHLr Register8          -- load the value8 stored in a register8 into the address in HL
  | LDHLn Word8              -- load the immediate value8 into the address in HL
  | LDABC                    -- load the value8 stored in the address in BC into A
  | LDADE                    -- load the value8 stored in the address in DE into A
  | LDAnn Word16             -- load the value8 stored in the value16 address into A
  | LDBCA                    -- load the value8 stored in A into the address in BC 
  | LDDEA                    -- load the value8 stored in A into the address in DE
  | LDnnA                    -- load the value8 stored in A into the value16 address

  | LDHLnn Word16            -- load the value16 address into the register16 HL
{- 
  GMB 8bit load commands

  ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
  ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
  ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
  ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
  ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
  ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
  ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
  ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1 
  
  
GMB 16bit-Loadcommands
  ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
  ld   SP,HL       F9         8 ---- SP=HL
  push rr          x5        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
  pop  rr          x1        12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
-}

  deriving (Read, Eq)
