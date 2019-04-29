{-|
    Module      : Lazyboy
    Description : Hardware definitions for Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    This module defines datatypes for the various aspects of the target hardware
    including registers and instructions.
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

  -- RGBASM-specific convenience stuff.
  -- these would need revamping if we were to start generating native machine code
  | LABEL Label               -- create a numbered label
  | JUMP Label                -- jump to a label
  | JUMPif Condition Label    -- conditional jumping to a label

  deriving (Read, Eq)

withLabel :: Lazyboy () -> Lazyboy ()
withLabel block = do
  label <- get
  modify (+ 1) -- increment the label name counter
  tell [LABEL $ Global label] 
  block

withLocalLabel :: Lazyboy () -> Lazyboy ()
withLocalLabel block = do
  label <- get
  modify (+ 1) -- increment the label name counter
  tell [LABEL $ Local label]
  block


loop :: Lazyboy () -> Lazyboy ()
loop block = do
  label <- get
  modify (+ 1)
  tell [LABEL $ Local label]
  block
  tell [JUMP $ Local label]

cond :: Condition -> Lazyboy () -> Lazyboy ()
cond condition block = do
  label <- get
  modify (+ 1)
  tell [JUMPif condition $ Local label]
  block
  tell [LABEL $ Local label]
