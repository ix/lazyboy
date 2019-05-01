{-|
    Module      : Lazyboy
    Description : Hardware definitions for Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    This module defines datatypes for the various aspects of the target hardware
    including registers and instructions. It also includes control flow functions.
-}

module Lazyboy where

import           Control.Monad                   (replicateM)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.RWS.Lazy
import           Data.Int
import           Data.Word

-- Rename and re-export RWS types and functions
type Lazyboy a = RWS () [Instruction] Integer a

execLazyboy :: Lazyboy a -> [Instruction]
execLazyboy m = snd $ execRWS m () 1

-- | Condition codes
data Condition = Zero | NonZero | Carry | NoCarry
  deriving (Read, Show, Eq)

-- | 8 bit registers
data Register8 = A | B | C | D | E | H | L
  deriving (Read, Show, Eq)

-- | 16 bit registers
data Register16 = BC | DE | HL | AF | SP | PC
  deriving (Read, Show, Eq)

data Label = Local Integer | Global Integer
  deriving (Read, Show, Eq)

-- | GB Opcodes and other special forms
data Instruction =
    LDrr Register8 Register8 -- load the value in one register8 into another
  | LDrn Register8 Word8     -- load the immediate value8 into a register8
  | LDrHL Register8          -- load the value8 stored at the address in HL into a register8
  | LDHLr Register8          -- load the value8 stored in a register8 into the address in HL
  | LDHLn Word8              -- load the immediate value8 into the address in HL
  | LDArr Register16         -- load the value at the address contained in a 16 bit register into A
  | LDrrA Register16         -- laod A into the address contained in a 16 bit register
  | LDAnn Word16             -- load the value8 stored in the value16 address into A
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
  
  -- Jump & Call instructions
  | JPnn Word16              -- immediately jump to value16
  | JPHL                     -- immediately jump to the value contained in HL
  | JPif Condition Word16    -- conditional jump to value16
  | JRPC Int8                -- relative jump by adding signed value8 to program counter
  | JRPCif Condition Int8    -- conditional jump to signed value8 + PC
  | CALL Word16              -- call the address
  | CALLif Condition Word16  -- conditional call to address
  | RET                      -- return
  | RETif Condition          -- conditional return
  | RETi                     -- return and enable interrupts
  | RST Word8                -- call a restart vector

  -- Arithmetic & Logical instructions
  | ADDAr Register8          -- add the value contained in a register to A
  | ADDAn Word8              -- add a value8 to the value contained in A
  | ADDHL                    -- add the value contained in the address stored in HL to A
  | ADCAr Register8          -- add the value in the register + the carry flag to A
  | ADCAn Word8              -- add the immediate value + the carry flag to A
  | ADCHL                    -- add the value contained in the address in HL + the carry flag to A
  | SUBAr Register8          -- subtract the value contained in a register from A
  | SUBAn Word8              -- subtract a value8 from A
  | SUBHL                    -- subtract from A the value contained in the address in HL
  | SBCAr Register8          -- subtract from A the value contained in the register + carry flag
  | SBCAn Word8              -- subtract from A the value + carry flag
  | SBCAHL                   -- subtract from A the value contained in the address in HL + carry flag
  | ANDr Register8           -- assign to A the value contained in a register & itself
  | ANDn Word8               -- assign to A a value8 & itself
  | ANDHL                    -- assign to A itself & the value in the address in HL
  | XORr Register8           -- assign to A the value contained in a register ^ itself
  | XORn Word8               -- assign to A a value8 ^ itself
  | XORHL                    -- assign to A itself ^ the value in the address in HL
  | ORr Register8            -- assign to A the value contained in a register | itself
  | ORn Word8                -- assign to A a value8 | itself
  | ORHL                     -- assign to A itself | the value in the address in HL
  | CPr Register8            -- substract from A the value in a register and set flags accordingly, don't store the result
  | CPn Word8                -- subtract from A a value8 and set flags accordingly, but don't store the result
  | CPHL                     -- subtract from A the value in the address in HL, set flags, don't store the result
  | INCr Register8           -- increment the value in a register
  | INCHL                    -- increment the value at the address in HL
  | DECr Register8           -- decrement the value in a register
  | DECHL                    -- decrement the value at the address in HL
  | DAA                      -- decimal-adjust register A
  | CPL                      -- complement accumulator (A = ~A)
  | ADDHLrr Register16       -- add the value contained in a 16 bit register to HL
  | INCrr Register16         -- increment the value in a 16 bit register
  | DECrr Register16         -- decrement the value in a 16 bit register 
  | ADDSPn Int8              -- add the signed value8 to the stack pointer
  | LDHLSPn Int8             -- load into hl the stack pointer + a value8

  -- TODO: Rotate, Shift, Single-bit instructions
  -- CPU control instructions
  | CCF                      -- complement carry flag
  | SCF                      -- set carry flag
  | NOP                      -- no operation
  | HALT                     -- halt until interrupt
  | STOP                     -- standby mode
  | DI                       -- disable interrupts
  | EI                       -- enable interrupts

  {-
    ccf            3F           4 -00c cy=cy xor 1
    scf            37           4 -001 cy=1
    nop            00           4 ---- no operation
    halt           76         N*4 ---- halt until interrupt occurs (low power)
    stop           10 00        ? ---- low power standby mode (VERY low power)
    di             F3           4 ---- disable interrupts, IME=0
    ei             FB           4 ---- enable interrupts, IME=1
  -}

  -- RGBASM-specific convenience stuff.
  -- these would need revamping if we were to start generating native machine code
  | LABEL Label               -- create a numbered label
  | JUMP Label                -- jump to a label
  | JUMPif Condition Label    -- conditional jumping to a label

  deriving (Read, Eq)

-- | Execute an action within a global label and pass the action the label.
withLabel :: (Label -> Lazyboy ()) -> Lazyboy ()
withLabel block = do
  label <- Global <$> get
  modify (+ 1) -- increment the label name counter
  tell [LABEL label] 
  block label

-- | Execute an action within a local label and pass the action the label.
withLocalLabel :: (Label -> Lazyboy ()) -> Lazyboy ()
withLocalLabel block = do
  label <- Local <$> get
  modify (+ 1) -- increment the label name counter
  tell [LABEL label]
  block label

-- | Suspend execution indefinitely by jumping infinitely.
freeze :: Lazyboy ()
freeze = loop $ return ()
  where loop block = do
          label <- Local <$> get
          modify (+ 1)
          tell [LABEL label]
          block
          tell [JUMP label]

-- | Executes the given action provided condition flag is set.
cond :: Condition -> Lazyboy () -> Lazyboy ()
cond condition block = do
  label <- Local <$> get
  modify (+ 1)
  tell [JUMPif condition label]
  block
  tell [LABEL label]
