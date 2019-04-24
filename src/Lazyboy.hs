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
data Register16 = BC | DE | HL | AF | SP | PC 
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
  | LDnnA Word16             -- load the value8 stored in A into the value16 address
  | LDAIO Word8              -- read into A from IO port n (FF00 + value8)
  | LDIOA Word8              -- store value8 in A into IO port n (FF00 + value8)
  | LDAIOC                   -- read from IO port FF00+c into A
  | LDIOCA                   -- store the value8 in A into IO port FF00+C
  | LDHLAI                   -- store value in register A into byte pointed by HL and post-increment HL
  | LDAHLI                   -- store value in address in HL in A and post-increment HL
  | LDHLAD                   -- store value in register A into byte pointed by HL and post-decrement HL.
  | LDAHLD                   -- store value in address in HL in A and post-decrement HL
  | LDrrnn Register16 Word16 -- load the value16 address into the register16
  | LDSPHL                   -- set the stack pointer to the value in HL
  | PUSH Register16          -- push register16 onto the stack
  | POP Register16           -- pop register16 from the stack

  deriving (Read, Eq)
