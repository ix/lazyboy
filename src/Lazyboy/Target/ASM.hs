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
import           Text.Printf                     (PrintfArg, printf)

-- | Format Opcodes as Strings
instance Show Opcode where
    show (LDrr r1 r2) = printf "ld %s, %s" (format r1) (format r2)
    show (LDrn r1 v1) = printf "ld %s, $%X" (format r1) v1
    show (LDrHL r1) = printf "ld %s, [HL]" (format r1)
    show (LDHLr r1) = printf "ld [HL], %s" (format r1)
    show (LDHLn v1) = printf "ld [HL], $%X" v1
    
    show (LDHLnn v1) = printf "ld HL, $%X" v1
    show _ = printf "unimplemented"

-- | Format a value by converting it to a lowercase String
format :: Show a => a -> String
format = map toLower . show

compileROM :: Writer [Opcode] a -> IO Text
compileROM code = do
    tem <- compileMustacheFile "templates/bare.mustache"
    return $ renderMustache tem $ object [ "body" .= body ]
    where body = map show $ execWriter code
