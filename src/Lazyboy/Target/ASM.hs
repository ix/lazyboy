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

import           Control.Exception
import           Data.List                    (intercalate)
import           Data.Text                    (Text)
import           Data.Word
import           Lazyboy.Types
import           Lazyboy.Templates            (templatize, basic)
import           Text.Printf

import qualified Data.Text                    as T

-- | Lazyboy exception type.
data LazyboyException =
    InvalidStackOperation
  | InvalidALoad Register16
  | AttemptedAFPCLoad
  | InvalidRSTVector
  | IllegalHLAddition
  | IllegalModification Register16
  | IntegerBoundsViolation Word8
  | Unimplemented

-- | An instance of Show for printing exception messages.
instance Show LazyboyException where
    show InvalidStackOperation  = printf "You cannot push or pop the program counter or stack pointer onto/from the stack."
    show (InvalidALoad r) =  printf "You cannot perform loads between the 16 bit register '%s' and A." r
    show AttemptedAFPCLoad = printf "You cannot load a 16 bit value directly into AF or the program counter."
    show InvalidRSTVector = "Invalid RST vector specified!"
    show IllegalHLAddition = "Cannot add the given the 16 bit register to HL."
    show (IllegalModification r) = printf "Cannot increment or decrement the given 16 bit register '%s'." r
    show (IntegerBoundsViolation v) = printf "Value '%d' given to an instruction expecting a 3-bit value." v
    show Unimplemented = "Use of an unimplemented instruction."

-- | An instance of Exception itself for LazyboyException.
instance Exception LazyboyException where

-- | A custom Show instance which formats Instructions as assembly.
instance Show Instruction where
    show (LDrr r1 r2) = printf "ld %s, %s" r1 r2
    show (LDrn r1 v1) = printf "ld %s, %d" r1 v1
    show (LDrHL r1)   = printf "ld %s, [HL]" r1
    show (LDHLr r1)   = printf "ld [HL], %s" r1
    show (LDHLn v1)   = printf "ld [HL], %d" v1
    show (LDArr BC)   = printf "ld A, [BC]"
    show (LDArr DE)   = printf "ld A, [DE]"
    show (LDArr HL)   = printf "ld A, [HL]"
    show (LDArr r1)   = throw $ InvalidALoad r1
    show (LDrrA BC)   = printf "ld [BC], A"
    show (LDrrA DE)   = printf "ld [DE], A"
    show (LDrrA HL)   = printf "ld [HL], A"
    show (LDrrA r1)   = throw $ InvalidALoad r1
    show (LDAnn v1)   = printf "ld A, [%s]" v1
    show (LDnnA v1)   = printf "ld [%s], A" v1
    show (LDAIO v1)   = printf "ldh A, [$FF00+$%X]" v1
    show (LDIOA v1)   = printf "ldh [$FF00+$%X], A" v1
    show (LDAIOC)     = printf "ldh A, [$FF00+C]"
    show (LDIOCA)     = printf "ldh [$FF00+C], A"
    show (LDHLAI)     = printf "ld [HL+], A"
    show (LDAHLI)     = printf "ld A, [HL+]"

    -- handle some special cases for ld rr,nn
    show (LDrrnn AF _) = throw AttemptedAFPCLoad
    show (LDrrnn PC _) = throw AttemptedAFPCLoad
    show (LDrrnn r1 v1)  = printf "ld %s, %s" r1 v1

    show (LDSPHL) = printf "ld SP, HL"

    -- stack manipulation
    show (PUSH SP) = throw InvalidStackOperation
    show (PUSH PC) = throw InvalidStackOperation
    show (PUSH r1) = printf "PUSH %s" r1

    show (POP SP) = throw InvalidStackOperation
    show (POP PC) = throw InvalidStackOperation
    show (POP r1) = printf "POP %s" r1

    -- jumps
    show (JP v1@(Address _)) = printf "jp %s" v1
    show (JP v1@(Name (Global _))) = printf "jp %s" v1
    show (JP v1@(Name (Local _))) = printf "jr %s" v1
    show (JPHL) = printf "jp HL"
    show (JPif c v1@(Address _)) = printf "jp %s, %s" c v1
    show (JPif c v1@(Name (Global _))) = printf "jp %s, %s" c v1
    show (JPif c v1@(Name (Local _))) = printf "jr %s, %s" c v1

    -- call and return
    show (CALL v1) = printf "call %s" v1
    show (CALLif c v1) = printf "call %s, %s" c v1
    show (RET) = printf "ret"
    show (RETif c) = printf "ret %s" c
    show (RETi) = printf "reti"

    show (RST 0x00) = printf "RST $00"
    show (RST 0x08) = printf "RST $08"
    show (RST 0x10) = printf "RST $10"
    show (RST 0x18) = printf "RST $18"
    show (RST 0x20) = printf "RST $20"
    show (RST 0x28) = printf "RST $28"
    show (RST 0x30) = printf "RST $30"
    show (RST 0x38) = printf "RST $38"
    show (RST _) = throw InvalidRSTVector

    -- arithmetic and comparisons
    show (ADDAr r1) = printf "add A, %s" r1
    show (ADDAn v) = printf "add A, %d" v
    show (ADDHL) = printf "add A, [HL]"
    show (ADCAr r1) = printf "adc A, %s" r1
    show (ADCAn v) = printf "adc A, %d" v
    show (ADCHL) = printf "adc A, [HL]"
    show (SUBAr r1) = printf "sub A, %s" r1
    show (SUBAn v) = printf "sub A, %d" v
    show (SUBHL) = printf "sub A, [HL]"
    show (SBCAr r1) = printf "sbc A, %s" r1
    show (SBCAn v) = printf "sbc A, %d" v
    show (SBCAHL) = printf "sbc A, [HL]"

    show (ANDr r1) = printf "and A, %s" r1
    show (ANDn v) = printf "and A, %d" v
    show (ANDHL) = printf "and A, [HL]"
    show (XORr r1) = printf "xor A, %s" r1
    show (XORn v) = printf "xor A, %d" v
    show (XORHL) = printf "xor A, [HL]"
    show (ORr r1) = printf "or A, %s" r1
    show (ORn v) = printf "or A, %d" v
    show (ORHL) = printf "or A, [HL]"
    show (CPr r1) = printf "cp A, %s" r1
    show (CPn v) = printf "cp A, %d" v
    show (CPHL) = printf "cp A, [HL]"
    show (INCr r1) = printf "inc %s" r1
    show (INCHL) = printf "inc [HL]"
    show (DECr r1) = printf "dec %s" r1
    show (DECHL) = printf "dec [HL]"
    show (DAA) = printf "daa"
    show (CPL) = printf "cpl"
    show (ADDHLrr BC) = printf "add HL, BC"
    show (ADDHLrr DE) = printf "add HL, DE"
    show (ADDHLrr HL) = printf "add HL, HL"
    show (ADDHLrr SP) = printf "add HL, SP"
    show (ADDHLrr _) = throw IllegalHLAddition
    show (INCrr BC) = printf "inc BC"
    show (INCrr DE) = printf "inc DE"
    show (INCrr HL) = printf "inc HL"
    show (INCrr SP) = printf "inc SP"
    show (INCrr r1) = throw $ IllegalModification r1
    show (DECrr BC) = printf "dec BC"
    show (DECrr DE) = printf "dec DE"
    show (DECrr HL) = printf "dec HL"
    show (DECrr SP) = printf "dec SP"
    show (DECrr r1) = throw $ IllegalModification r1

    -- Rotate & shift
    show (RLCA) = printf "rlca"
    show (RLA) = printf "rla"
    show (RRCA) = printf "rrca"
    show (RRA) = printf "rra"
    show (RLC r1) = printf "rlc %s" r1
    show (RLCHL) = printf "rlc [HL]"
    show (RL r1) = printf "rl %s" r1
    show (RLHL) = printf "rl [HL]"
    show (RRC r1) = printf "rrc %s" r1
    show (RRCHL) = printf "rrc [HL]"
    show (RR r1) = printf "rr %s" r1
    show (RRHL) = printf "rr [HL]"
    show (SLA r1) = printf "sla %s" r1
    show (SLAHL) = printf "sla [HL]"
    show (SWAP r1) = printf "swap %s" r1
    show (SWAPHL) = printf "swap [HL]"
    show (SRA r1) = printf "sra %s" r1
    show (SRAHL) = printf "sra [HL]"
    show (SRL r1) = printf "srl %s" r1
    show (SRLHL) = printf "srl [HL]"

    -- CPU control
    show (CCF) = printf "ccf"
    show (SCF) = printf "scf"
    show (NOP) = printf "nop"
    show (HALT) = printf "halt"
    show (STOP) = printf "stop"
    show (DI) = printf "di"
    show (EI) = printf "ei"

    -- Bit manipulation
    show (BITnr v r1)
        | v >= 0 && v <= 7 = printf "bit %d, %s" v r1
        | otherwise        = throw $ IntegerBoundsViolation v
    show (BITnHL v)
        | v >= 0 && v <= 7 = printf "bit %d, HL" v
        | otherwise        = throw $ IntegerBoundsViolation v
    show (SETnr v r1)
        | v >= 0 && v <= 7 = printf "set %d, %s" v r1
        | otherwise        = throw $ IntegerBoundsViolation v
    show (SETnHL v)
        | v >= 0 && v <= 7 = printf "set %d, HL" v
        | otherwise        = throw $ IntegerBoundsViolation v
    show (RESnr v r1)
        | v >= 0 && v <= 7 = printf "res %d, %s" v r1
        | otherwise        = throw $ IntegerBoundsViolation v
    show (RESnHL v)
        | v >= 0 && v <= 7 = printf "res %d, HL" v
        | otherwise        = throw $ IntegerBoundsViolation v

    -- RGBASM specific stuff
    show (LABEL l) = printf "%s:" l
    show (INCLUDE file) = printf "INCBIN \"%s\"" file
    show (BYTES bytes) = printf "db " ++ intercalate "," (map (printf "$%X") bytes)

    show _            = throw Unimplemented

-- | Instances of PrintfArg
instance PrintfArg Register16 where
    formatArg = formatString . show

instance PrintfArg Register8 where
    formatArg = formatString . show

instance PrintfArg Condition where
    formatArg Zero    = formatString "z"
    formatArg NonZero = formatString "nz"
    formatArg Carry   = formatString "c"
    formatArg NoCarry = formatString "nc"

instance PrintfArg Label where
    formatArg (Local v)  = formatString $ ".L" ++ show v
    formatArg (Global v) = formatString $ "L" ++ show v

instance PrintfArg Location where
    formatArg (Address v)  = formatString $ (printf "$%X" v :: String)
    formatArg (Name label) = formatString $ (printf "%s" label :: String)

-- | Compiles an action to an assembly source file.
-- This function makes use of a "bare" template, which
-- sets up an appropriate start location for the body of the program
-- and defines an entry point label 'main'.
compileROM :: Lazyboy a -> Text
compileROM code = templatize basic body
  where body = T.unlines $ map (T.pack . show) $ execLazyboy code
