{-|
    Module      : Lazyboy.Target.ASM
    Description : ASM backend for Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    This module provides a backend to format opcodes as ASM and
    produce assembly files which are then buildable into ROMs (with RGBDS).
-}

{-# LANGUAGE OverloadedStrings #-}

module Lazyboy.Target.ASM where

import Control.Monad.Trans.Except (Except, throwE, runExcept)
import Data.Text                  (Text)
import Data.Word                  (Word8)
import Formatting                 (sformat, formatToString, (%))
import Formatting.ShortFormatters (sh, x, d)
import Lazyboy.Templates          (basic, templatize)
import Lazyboy.Types

import qualified Data.Text as T
import qualified Data.Text.IO as T

class Compile a where
  compile :: a -> Except CompileError Text

-- | Compilation errors.
data CompileError =
    InvalidStackOperation
  | InvalidALoad Register16
  | AttemptedAFPCLoad
  | InvalidRSTVector
  | IllegalHLAddition
  | IllegalModification Register16
  | IntegerBoundsViolation Word8
  | Unimplemented
  deriving (Eq)

-- | An instance of Show for printing compiler errors.
instance Show CompileError where
    show InvalidStackOperation = "You cannot push or pop the program counter or stack pointer onto/from the stack."
    show (InvalidALoad r) = formatToString ("You cannot perform loads between the 16 bit register '" % sh % "' and A.") r
    show AttemptedAFPCLoad = "You cannot load a 16 bit value directly into AF or the program counter."
    show InvalidRSTVector = "Invalid RST vector specified!"
    show IllegalHLAddition = "Cannot add the given the 16 bit register to HL."
    show (IllegalModification r) = formatToString ("Cannot increment or decrement the given 16 bit register '" % sh % "'.") r
    show (IntegerBoundsViolation v) = formatToString ("Value '" % x % "' given to an instruction expecting a 3-bit value.") v
    show Unimplemented = "Use of an unimplemented instruction."

-- | A custom Show instance which formats Instructions as assembly.
instance Compile Instruction where
    compile (LDrr r1 r2) = pure $ sformat ("ld " % sh % ", " % sh) r1 r2
    compile (LDrn r1 v1) = pure $ sformat ("ld " % sh % ", " % d) r1 v1
    compile (LDrHL r1)   = pure $ sformat ("ld " % sh % ", [HL]") r1
    compile (LDHLr r1)   = pure $ sformat ("ld [HL], " % sh) r1
    compile (LDHLn v1)   = pure $ sformat ("ld [HL], " % d) v1
    compile (LDArr BC)   = pure "ld A, [BC]"
    compile (LDArr DE)   = pure "ld A, [DE]"
    compile (LDArr HL)   = pure "ld A, [HL]"
    compile (LDArr r1)   = throwE $ InvalidALoad r1
    compile (LDrrA BC)   = pure "ld [BC], A"
    compile (LDrrA DE)   = pure "ld [DE], A"
    compile (LDrrA HL)   = pure "ld [HL], A"
    compile (LDrrA r1)   = throwE $ InvalidALoad r1
    compile (LDAnn v1)   = pure $ sformat ("ld A, [" % sh % "]") v1
    compile (LDnnA v1)   = pure $ sformat ("ld [" % sh % "], A") v1
    compile (LDAIO v1)   = pure $ sformat ("ldh A, [$FF00+$" % x % "]") v1
    compile (LDIOA v1)   = pure $ sformat ("ldh [$FF00+$" % x % "], A") v1
    compile LDAIOC       = pure "ldh A, [$FF00+C]"
    compile LDIOCA       = pure "ldh [$FF00+C], A"
    compile LDHLAI       = pure "ld [HL+], A"
    compile LDAHLI       = pure "ld A, [HL+]"

    -- handle some special cases for ld rr,nn
    compile (LDrrnn AF _) = throwE AttemptedAFPCLoad
    compile (LDrrnn PC _) = throwE AttemptedAFPCLoad
    compile (LDrrnn r1 v1)  = pure $ sformat ("ld " % sh % ", " % sh) r1 v1

    compile LDSPHL = pure "ld SP, HL"

    -- stack manipulation
    compile (PUSH SP) = throwE InvalidStackOperation
    compile (PUSH PC) = throwE InvalidStackOperation
    compile (PUSH r1) = pure $ sformat ("PUSH " % sh) r1

    compile (POP SP) = throwE InvalidStackOperation
    compile (POP PC) = throwE InvalidStackOperation
    compile (POP r1) = pure $ sformat ("POP " % sh) r1

    -- jumps
    compile (JP v1@(Address _)) = pure $ sformat ("jp " % sh) v1
    compile (JP v1@(Name (Global _))) = pure $ sformat ("jp " % sh) v1
    compile (JP v1@(Name (Local _))) = pure $ sformat ("jr " % sh) v1
    compile JPHL = pure "jp HL"
    compile (JPif c v1@(Address _)) = pure $ sformat ("jp " % sh % ", " % sh) c v1
    compile (JPif c v1@(Name (Global _))) = pure $ sformat ("jp " % sh % ", " % sh) c v1
    compile (JPif c v1@(Name (Local _))) = pure $ sformat ("jr " % sh % ", " % sh) c v1

    -- call and return
    compile (CALL v1) = pure $ sformat ("call " % sh) v1
    compile (CALLif c v1) = pure $ sformat ("call " % sh % ", " % sh) c v1
    compile RET = pure "ret"
    compile (RETif c) = pure $ sformat ("ret " % sh) c
    compile RETi = pure "reti"

    compile (RST 0x00) = pure "RST $00"
    compile (RST 0x08) = pure "RST $08"
    compile (RST 0x10) = pure "RST $10"
    compile (RST 0x18) = pure "RST $18"
    compile (RST 0x20) = pure "RST $20"
    compile (RST 0x28) = pure "RST $28"
    compile (RST 0x30) = pure "RST $30"
    compile (RST 0x38) = pure "RST $38"
    compile (RST _) = throwE InvalidRSTVector

    -- arithmetic and comparisons
    compile (ADDAr r1) = pure $ sformat ("add A, " % sh) r1
    compile (ADDAn v) = pure $ sformat ("add A, " % d) v
    compile ADDHL = pure "add A, [HL]"
    compile (ADCAr r1) = pure $ sformat ("adc A, " % sh) r1
    compile (ADCAn v) = pure $ sformat ("adc A, " % d) v
    compile ADCHL = pure  "adc A, [HL]"
    compile (SUBAr r1) = pure $ sformat ("sub A, " % sh) r1
    compile (SUBAn v) = pure $ sformat ("sub A, " % d) v
    compile SUBHL = pure "sub A, [HL]"
    compile (SBCAr r1) = pure $ sformat ("sbc A, " % sh) r1
    compile (SBCAn v) = pure $ sformat ("sbc A, " % d) v
    compile SBCAHL = pure "sbc A, [HL]"

    compile (ANDr r1) = pure $ sformat ("and A, " % sh) r1
    compile (ANDn v) = pure $ sformat ("and A, " % d) v
    compile ANDHL = pure "and A, [HL]"
    compile (XORr r1) = pure $ sformat ("xor A, " % sh) r1
    compile (XORn v) = pure $ sformat ("xor A, " % d) v
    compile XORHL = pure "xor A, [HL]"
    compile (ORr r1) = pure $ sformat ("or A, " % sh) r1
    compile (ORn v) = pure $ sformat ("or A, " % d) v
    compile ORHL = pure "or A, [HL]"
    compile (CPr r1) = pure $ sformat ("cp A, " % sh) r1
    compile (CPn v) = pure $ sformat ("cp A, " % d) v
    compile CPHL = pure "cp A, [HL]"
    compile (INCr r1) = pure $ sformat ("inc " % sh) r1
    compile INCHL = pure "inc [HL]"
    compile (DECr r1) = pure $ sformat ("dec " % sh) r1
    compile DECHL = pure "dec [HL]"
    compile DAA = pure "daa"
    compile CPL = pure "cpl"
    compile (ADDHLrr BC) = pure "add HL, BC"
    compile (ADDHLrr DE) = pure "add HL, DE"
    compile (ADDHLrr HL) = pure "add HL, HL"
    compile (ADDHLrr SP) = pure "add HL, SP"
    compile (ADDHLrr _) = throwE IllegalHLAddition
    compile (INCrr BC) = pure "inc BC"
    compile (INCrr DE) = pure "inc DE"
    compile (INCrr HL) = pure "inc HL"
    compile (INCrr SP) = pure "inc SP"
    compile (INCrr r1) = throwE $ IllegalModification r1
    compile (DECrr BC) = pure "dec BC"
    compile (DECrr DE) = pure "dec DE"
    compile (DECrr HL) = pure "dec HL"
    compile (DECrr SP) = pure "dec SP"
    compile (DECrr r1) = throwE $ IllegalModification r1

    -- Rotate & shift
    compile RLCA = pure "rlca"
    compile RLA = pure "rla"
    compile RRCA = pure "rrca"
    compile RRA = pure "rra"
    compile (RLC r1) = pure $ sformat ("rlc " % sh) r1
    compile RLCHL = pure "rlc [HL]"
    compile (RL r1) = pure $ sformat ("rl " % sh) r1
    compile RLHL = pure "rl [HL]"
    compile (RRC r1) = pure $ sformat ("rrc " % sh) r1
    compile RRCHL = pure "rrc [HL]"
    compile (RR r1) = pure $ sformat ("rr " % sh) r1
    compile RRHL = pure "rr [HL]"
    compile (SLA r1) = pure $ sformat ("sla " % sh) r1
    compile SLAHL = pure "sla [HL]"
    compile (SWAP r1) = pure $ sformat ("swap " % sh) r1
    compile SWAPHL = pure "swap [HL]"
    compile (SRA r1) = pure $ sformat ("sra " % sh) r1
    compile SRAHL = pure "sra [HL]"
    compile (SRL r1) = pure $ sformat ("srl " % sh) r1
    compile SRLHL = pure "srl [HL]"

    -- CPU control
    compile CCF = pure "ccf"
    compile SCF = pure "scf"
    compile NOP = pure "nop"
    compile HALT = pure "halt"
    compile STOP = pure "stop"
    compile DI = pure "di"
    compile EI = pure "ei"

    -- Bit manipulation
    compile (BITnr v r1)
        | v >= 0 && v <= 7 = pure $ sformat ("bit " % d % ", " % sh) v r1
        | otherwise        = throwE $ IntegerBoundsViolation v
    compile (BITnHL v)
        | v >= 0 && v <= 7 = pure $ sformat ("bit " % d % ", HL") v
        | otherwise        = throwE $ IntegerBoundsViolation v
    compile (SETnr v r1)
        | v >= 0 && v <= 7 = pure $ sformat ("set " % d % ", " % sh) v r1
        | otherwise        = throwE $ IntegerBoundsViolation v
    compile (SETnHL v)
        | v >= 0 && v <= 7 = pure $ sformat ("set " % d % ", HL") v
        | otherwise        = throwE $ IntegerBoundsViolation v
    compile (RESnr v r1)
        | v >= 0 && v <= 7 = pure $ sformat ("res " % d % ", " % sh) v r1
        | otherwise        = throwE $ IntegerBoundsViolation v
    compile (RESnHL v)
        | v >= 0 && v <= 7 = pure $ sformat ("res " % d % ", HL") v
        | otherwise        = throwE $ IntegerBoundsViolation v

    -- RGBASM specific stuff
    compile (LABEL l) = pure $ sformat (sh % ":") l
    compile (INCLUDE file) = pure $ sformat ("INCBIN \"" % sh % "\"") file
    compile (BYTES bytes) = pure $ sformat "db " <> T.intercalate "," (map (sformat ("$" % x)) bytes)

    compile _            = throwE Unimplemented

-- | Compiles an action to an assembly source file.
-- This function makes use of a "bare" template, which
-- sets up an appropriate start location for the body of the program
-- and defines an entry point label 'main'.
compileROM :: Lazyboy a -> Except CompileError Text
compileROM = fmap (templatize basic . T.unlines) . mapM compile . execLazyboy

-- | Compile to a ROM, and print either the output assembly or the compile errors if any.
lazyboy :: Lazyboy a -> IO ()
lazyboy program = either print T.putStrLn $ runExcept $ compileROM program
