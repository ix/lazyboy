{-|
    Module      : Lazyboy.Types
    Description : Hardware type definitions for Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    This module defines datatypes for the various aspects of the target hardware
    including registers and instructions.
-}

module Lazyboy.Types where

import           Control.Monad                (replicateM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.RWS.Lazy
import           Data.Int
import           Data.Word

-- | A type alias that defines Lazyboy as a specialization of the
-- RWS monad transformer stack. Reader goes unused, Writer is utilized
-- for an output list of Instructions, and State is merely an integer
-- which counts labels, thus naming them.
type Lazyboy a = RWS () [Instruction] Integer a

-- | Executes an action and returns a list of Instructions.
execLazyboy :: Lazyboy a -> [Instruction]
execLazyboy m = snd $ execRWS m () 1

-- | A type which represents an address or label.
data Location = Address Word16 | Name Label
  deriving (Eq)

-- | A type representing Condition flags on the hardware.
data Condition = Zero | NonZero | Carry | NoCarry
  deriving (Eq)

-- | Named 8-bit registers.
data Register8 = A | B | C | D | E | H | L
  deriving (Read, Show, Eq)

-- | Named 16-bit registers.
data Register16 = BC | DE | HL | AF | SP | PC
  deriving (Read, Show, Eq)

-- | A type which represents a label, which may be local or global in scope.
data Label = Local Integer | Global Integer
  deriving (Eq)

-- | Type-level representations of instructions and primitive special forms.
data Instruction = 
    LDrr Register8 Register8   -- ^ Load the value in one Register8 into another.
  | LDrn Register8 Word8       -- ^ Load the immediate Word8 into a Register8.
  | LDrHL Register8            -- ^ Load the Word8 stored at the address in HL into a Register8.
  | LDHLr Register8            -- ^ Load the Word8 stored in a Register8 into the address in HL.
  | LDHLn Word8                -- ^ Load the immediate Word8 into the address in HL.
  | LDArr Register16           -- ^ Load the value at the address contained in a Register16 into A.
  | LDrrA Register16           -- ^ Load A into the address contained in a Register16.
  | LDAnn Location             -- ^ Load the Word8 stored in the Location into A.
  | LDnnA Location             -- ^ Load the Word8 stored in A into the Location.
  | LDAIO Word8                -- ^ Read into A from IO port n (FF00 + Word8).
  | LDIOA Word8                -- ^ Store the Word8 in A into IO port n (FF00 + Word8).
  | LDAIOC                     -- ^ Read from IO port FF00+C into A.
  | LDIOCA                     -- ^ Store the Word8 in A into IO port FF00+C.
  | LDHLAI                     -- ^ Store value in register A into byte pointed by HL and post-increment HL.
  | LDAHLI                     -- ^ Store value in address in HL in A and post-increment HL.
  | LDHLAD                     -- ^ Store value in register A into byte pointed by HL and post-decrement HL.
  | LDAHLD                     -- ^ Store value in address in HL in A and post-decrement HL.
  | LDrrnn Register16 Location -- ^ Load a Location into a Register16.
  | LDSPHL                     -- ^ Set the stack pointer (SP) to the value in HL.
  | PUSH Register16            -- ^ Push Register16 onto the stack.
  | POP Register16             -- ^ Pop Register16 from the stack.

  -- Jump & Call instructions
  | JP Location               -- ^ Immediately and unconditionally jump to a Location.
  | JPHL                      -- ^ Immediately and unconditionally jump to the value contained in HL.
  | JPif Condition Location   -- ^ Conditionally jump to a Location.
  | CALL Location             -- ^ Call a Location.
  | CALLif Condition Location -- ^ Conditionally call a Location. 
  | RET                       -- ^ Return from a labelled block.
  | RETif Condition           -- ^ Conditionally return from a labelled block. 
  | RETi                      -- ^ Return and enable interrupts.
  | RST Word8                 -- ^ Call a restart vector.

  -- Arithmetic & Logical instructions
  | ADDAr Register8          -- ^ Add the value contained in a Register8 to A.
  | ADDAn Word8              -- ^ Add a Word8 to the value contained in A.
  | ADDHL                    -- ^ Add the value contained in the address stored in HL to A.
  | ADCAr Register8          -- ^ Add the value in a Register8 + the carry flag to A.
  | ADCAn Word8              -- ^ Add a Word8 + the carry flag to A.
  | ADCHL                    -- ^ Add the value pointed to by HL + the carry flag to A.
  | SUBAr Register8          -- ^ Subtract the value contained in a Register8 from A.
  | SUBAn Word8              -- ^ Subtract a Word8 from A.
  | SUBHL                    -- ^ Subtract from A the value contained at the address in HL.
  | SBCAr Register8          -- ^ Subtract from A the value contained in a Register8 + the carry flag.
  | SBCAn Word8              -- ^ Subtract from A a Word8 + the carry flag.
  | SBCAHL                   -- ^ Subtract from A the value contained in the address in HL + the carry flag.
  | ANDr Register8           -- ^ Assign to A the value contained in a Register8 & A itself.
  | ANDn Word8               -- ^ Assign to A a Word8 & A itself.
  | ANDHL                    -- ^ Assign to A itself & the value in the address in HL.
  | XORr Register8           -- ^ Assign to A the value contained in a register ^ A itself
  | XORn Word8               -- ^ Assign to A a Word8 ^ itself.
  | XORHL                    -- ^ Assign to A itself ^ the value in the address in HL.
  | ORr Register8            -- ^ Assign to A the value contained in a register | A itself.
  | ORn Word8                -- ^ Assign to A a Word8 | itself.
  | ORHL                     -- ^ Assign to A itself | the value in the address in HL
  | CPr Register8            -- ^ Subtract from A the value in a Register8 and set flags accordingly, but don't store the result.
  | CPn Word8                -- ^ Subtract from A a Word8 and set flags accordingly, but don't store the result.
  | CPHL                     -- ^ Subtract from A the value in the address in HL, set flags, but don't store the result.
  | INCr Register8           -- ^ Increment the value in a Register8.
  | INCHL                    -- ^ Increment the value at the address in HL.
  | DECr Register8           -- ^ Decrement the value in a Register8.
  | DECHL                    -- ^ Decrement the value at the address in HL.
  | DAA                      -- ^ Decimal-adjust register A.
  | CPL                      -- ^ Complement accumulator (A = ~A).
  | ADDHLrr Register16       -- ^ Add the value contained in a Register16 to HL.
  | INCrr Register16         -- ^ Increment the value in a Register16.
  | DECrr Register16         -- ^ Decrement the value in a Register16.
  | ADDSPn Int8              -- ^ Add an Int8 to the stack pointer.
  | LDHLSPn Int8             -- ^ Load into HL the stack pointer + an Int8.

  -- Single-bit instructions
  | BITnr Word8 Register8    -- ^ Test bit n in a Register8, set the zero flag if not set.
  | BITnHL Word8             -- ^ Test bit n in the Word8 pointed by HL, set the zero flag if not set.
  | SETnr Word8 Register8    -- ^ Set bit n in a Register8.
  | SETnHL Word8             -- ^ Set bit n in the Word8 pointed by HL.
  | RESnr Word8 Register8    -- ^ Unset bit n in Register8.
  | RESnHL Word8             -- ^ Unset bit n in the Word8 pointed by HL.

  -- Rotate & shift instructions
  | RLCA                     -- ^ Rotate accumulator left.
  | RLA                      -- ^ Rotate accumulator left through carry.
  | RRCA                     -- ^ Rotate accumulator right.
  | RRA                      -- ^ Rotate accumulator rit through carry.
  | RLC Register8            -- ^ Rotate Register8 left.
  | RLCHL                    -- ^ Rotate value contained at address in HL left.
  | RL Register8             -- ^ Rotate Register8 left through carry.
  | RLHL                     -- ^ Rotate value contained at address in HL left through carry.
  | RRC Register8            -- ^ Rotate Register8 right.
  | RRCHL                    -- ^ Rotate value contained at address in HL right.
  | RR Register8             -- ^ Rotate Register8 right through carry.
  | RRHL                     -- ^ Rotate value contained at address in HL right through carry.
  | SLA Register8            -- ^ Shift Register8 left arithmetic.
  | SLAHL                    -- ^ Shift left arithmetic (HL pointer).
  | SWAP Register8           -- ^ Exchange low and high nibbles in Register8.
  | SWAPHL                   -- ^ Exchange low and high nibbles in HL pointer.
  | SRA Register8            -- ^ Shift Register8 right arithmetic.
  | SRAHL                    -- ^ Shift right arithmetic in HL pointer.
  | SRL Register8            -- ^ Shift Register8 right logical.
  | SRLHL                    -- ^ Shift right logical in HL pointer.

  -- CPU control instructions
  | CCF                      -- ^ Complement carry flag.
  | SCF                      -- ^ Set carry flag.
  | NOP                      -- ^ No operation.
  | HALT                     -- ^ Halt until interrupt.
  | STOP                     -- ^ Standby mode.
  | DI                       -- ^ Disable interrupts.
  | EI                       -- ^ Enable interrupts.

  -- RGBASM-specific convenience stuff.
  -- these would need revamping if we were to start generating native machine code
  | LABEL Label               -- ^ Create a numbered label.
  | INCLUDE FilePath          -- ^ Include the file at FilePath.
  | BYTES [Word8]             -- ^ Define some bytes in the form of a Word8 list with a global label.

  deriving (Eq)