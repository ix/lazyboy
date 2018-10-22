module Control.Monad.Z80.ASM where

import           Control.Monad.Z80
import           Data.Char         (toLower)
import Text.Printf (printf, PrintfArg)

instance Show Z80 where
    show (LDreg r1 r2)     = mconcat ["ld ", rfmt r1, ", ", rfmt r2]
    show (LDimm reg val)   = mconcat ["ld ", rfmt reg, ", ", hex val]
    show (LD16imm reg val) = mconcat ["ld ", rfmt reg, ", ", hex val]
    show (LDHLreg reg)     = "ld [hl], " ++ rfmt reg
    show (LDHLimm val)     = "ld [hl]," ++ lowercase (hex val)
    show (LDregHL reg)     = "ld " ++ rfmt reg ++ ", [hl]"
    
hex :: PrintfArg a => a -> String
hex = printf "$%X"

rfmt :: Show a => a -> String
rfmt = lowercase . show

lowercase :: String -> String
lowercase = map toLower
