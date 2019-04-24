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

import           Control.Monad.Trans.Writer.Lazy
import           Data.Aeson
import           Data.Char                       (toLower)
import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy.IO               as T
import           Lazyboy
import           Text.Microstache
import           Text.Printf

-- | Format Opcodes as Strings
instance Show Opcode where
    show (LDrr r1 r2) = printf "ld %s, %s" r1 r2
    show (LDrn r1 v1) = printf "ld %s, $%X" r1 v1
    show (LDrHL r1)   = printf "ld %s, [HL]" r1
    show (LDHLr r1)   = printf "ld [HL], %s" r1
    show (LDHLn v1)   = printf "ld [HL], $%X" v1
    show (LDABC)      = printf "ld A, [BC]"
    show (LDADE)      = printf "ld A, [DE]"
    show (LDAnn v1)   = printf "ld A, [$%X]" v1
    show (LDBCA)      = printf "ld [BC], A"
    show (LDDEA)      = printf "ld [DE], A"
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

    show (PUSH SP) = error "You cannot push the stack pointer onto the stack"
    show (PUSH PC) = error "You cannot push the program counter onto the stack"
    show (PUSH r1) = printf "PUSH %s" r1

    show (POP SP) = error "You cannot pop the stack pointer from the stack"
    show (POP PC) = error "You cannot pop the program counter from the stack"
    show (POP r1) = printf "POP %s" r1

    show _            = error "Use of unimplemented instruction"

-- | Instances of PrintfArg for the registers
instance PrintfArg Register16 where
    formatArg = formatString . show

instance PrintfArg Register8 where
    formatArg = formatString . show

compileROM :: Writer [Opcode] a -> IO Text
compileROM code = do
    tem <- compileMustacheFile "templates/bare.mustache"
    return $ renderMustache tem $ object [ "body" .= body ]
    where body = map show $ execWriter code
