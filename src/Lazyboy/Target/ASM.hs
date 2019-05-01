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

import           Control.Monad.Trans.RWS.Lazy
import           Data.Aeson
import           Data.Char                    (toLower)
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy.IO            as T
import           Lazyboy
import           Paths_lazyboy
import           Text.Microstache
import           Text.Printf

-- | Format Instructions as Strings
instance Show Instruction where
    show (LDrr r1 r2) = printf "ld %s, %s" r1 r2
    show (LDrn r1 v1) = printf "ld %s, $%X" r1 v1
    show (LDrHL r1)   = printf "ld %s, [HL]" r1
    show (LDHLr r1)   = printf "ld [HL], %s" r1
    show (LDHLn v1)   = printf "ld [HL], $%X" v1
    show (LDArr BC)   = printf "ld A, [BC]"
    show (LDArr DE)   = printf "ld A, [DE]"
    show (LDArr HL)   = printf "ld A, [HL]"
    show (LDArr r1)   = error "16 bit register '%s' cannot be loaded into A" r1
    show (LDrrA BC)   = printf "ld [BC], A"
    show (LDrrA DE)   = printf "ld [DE], A"
    show (LDrrA HL)   = printf "ld [HL], A"
    show (LDrrA r1)   = error "A cannot be loaded into 16 bit register '%s'" r1
    show (LDAnn v1)   = printf "ld A, [$%X]" v1
    show (LDnnA v1)   = printf "ld [$%X], A" v1
    show (LDAIO v1)   = printf "ldh A, [$FF00+$%X]" v1
    show (LDIOA v1)   = printf "ldh [$FF00+$%X], A" v1
    show (LDAIOC)     = printf "ldh A, [$FF00+C]"
    show (LDIOCA)     = printf "ldh [$FF00+C], A"
    show (LDHLAI)     = printf "ld [HL+], A"
    show (LDAHLI)     = printf "ld A, [HL+]"

    -- handle some special cases for ld rr,nn
    show (LDrrnn AF _) = error "You cannot load a 16 bit value directly into the register AF"
    show (LDrrnn PC _) = error "You cannot load a 16 bit value directly into the program counter"
    show (LDrrnn r1 v1)  = printf "ld %s, $%X" r1 v1

    show (LDSPHL) = printf "%ld SP, HL"

    -- stack manipulation
    show (PUSH SP) = error "You cannot push the stack pointer onto the stack"
    show (PUSH PC) = error "You cannot push the program counter onto the stack"
    show (PUSH r1) = printf "PUSH %s" r1

    show (POP SP) = error "You cannot pop the stack pointer from the stack"
    show (POP PC) = error "You cannot pop the program counter from the stack"
    show (POP r1) = printf "POP %s" r1

    -- jumps
    show (JPnn v1) = printf "jp $%X" v1
    show (JPHL) = printf "jp HL"
    show (JPif c v1) = printf "jp %s, $%X" c v1
    show (JRPC v1) = printf "jr %d" v1
    show (JRPCif c v1) = printf "jr %s, $%X" c v1

    -- call and return
    show (CALL v1) = printf "call $%X" v1
    show (CALLif c v1) = printf "call %s, $%X" c v1
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
    show (RST _) = error "Invalid RST vector specified!"

    -- arithmetic and comparisons
    show (ADDAr r1) = printf "add A, %s" r1
    show (ADDAn v) = printf "add A, $%X" v
    show (ADDHL) = printf "add A, [HL]"
    show (ADCAr r1) = printf "adc A, %s" r1 
    show (ADCAn v) = printf "adc A, $%X" v
    show (ADCHL) = printf "adc A, [HL]"
    show (SUBAr r1) = printf "sub A, %s" r1
    show (SUBAn v) = printf "sub A, $%X" v
    show (SUBHL) = printf "sub A, [HL]"
    show (SBCAr r1) = printf "sbc A, %s" r1 
    show (SBCAn v) = printf "sbc A, $%X" v
    show (SBCAHL) = printf "sbc A, [HL]"
    
    show (ANDr r1) = printf "and A, %s" r1
    show (ANDn v) = printf "and A, $%X" v
    show (ANDHL) = printf "and A, [HL]"
    show (XORr r1) = printf "xor A, %s" r1
    show (XORn v) = printf "xor A, $%X" v
    show (XORHL) = printf "xor A, [HL]"
    show (ORr r1) = printf "or A, %s" r1
    show (ORn v) = printf "or A, $%X" v
    show (ORHL) = printf "or A, [HL]"
    show (CPr r1) = printf "cp A, %s" r1
    show (CPn v) = printf "cp A, $%X" v
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
    show (ADDHLrr r1) = error "Cannot add the given the 16 bit register to HL" 
    show (INCrr BC) = printf "inc BC"
    show (INCrr DE) = printf "inc DE"  
    show (INCrr HL) = printf "inc HL" 
    show (INCrr SP) = printf "inc SP"
    show (INCrr r1) = error "Cannot increment the given 16 bit register" 
    show (DECrr BC) = printf "dec BC"
    show (DECrr DE) = printf "dec DE"  
    show (DECrr HL) = printf "dec HL" 
    show (DECrr SP) = printf "dec SP"
    show (DECrr r1) = error "Cannot decrement the given 16 bit register" 

    -- RGBASM specific stuff
    show (LABEL l) = printf "%s:" l
    show (JUMP l) = printf "jp %s" l
    show (JUMPif c l) = printf "jp %s, %s" c l

    show _            = error "Use of unimplemented instruction"

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
    formatArg (Local v) = formatString $ ".L" ++ show v
    formatArg (Global v) = formatString $ "L" ++ show v

compileROM :: Lazyboy a -> IO Text
compileROM code = do
    templatePath <- getDataFileName "templates/bare.mustache"
    tem <- compileMustacheFile templatePath
    return $ renderMustache tem $ object [ "body" .= body ]
    where body = map show $ execLazyboy code
