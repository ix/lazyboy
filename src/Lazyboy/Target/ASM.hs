-- | This module provides a backend for Lazyboy which produces
--   Game Boy machine code.
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

-- | Format Ppcodes as Strings
instance Show Opcode where
    show (LDreg r1 r2)     = mconcat ["ld ", format r1, ", ", format r2]
    show (LDimm reg val)   = mconcat ["ld ", format reg, ", ", hexify val]
    show (LD16imm reg val) = mconcat ["ld ", format reg, ", ", hexify val]
    show (LDHLreg reg)     = "ld [hl], " ++ format reg
    show (LDHLimm val)     = "ld [hl]," ++ lowercase (hexify val)
    show (LDregHL reg)     = "ld " ++ format reg ++ ", [hl]"

-- | Convert a value to a hexadecimal representation
hexify :: PrintfArg a => a -> String
hexify = printf "$%X"

-- | Format a value by converting it to a lowercase String
format :: Show a => a -> String
format = lowercase . show

-- | Convert a String to lowercase
lowercase :: String -> String
lowercase = map toLower

compileROM :: Writer [Opcode] a -> IO Text
compileROM code = do
    tem <- compileMustacheFile "templates/bare.mustache"
    return $ renderMustache tem $ object [ "body" .= body ]
    where body = map show $ execWriter code
