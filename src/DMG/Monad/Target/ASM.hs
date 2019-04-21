module DMG.Monad.Target.ASM where

import           DMG.Monad
import           Data.Char         (toLower)
import Text.Printf (printf, PrintfArg)

instance Show Opcode where
    show (LDreg r1 r2)     = mconcat ["ld ", format r1, ", ", format r2]
    show (LDimm reg val)   = mconcat ["ld ", format reg, ", ", hexify val]
    show (LD16imm reg val) = mconcat ["ld ", format reg, ", ", hexify val]
    show (LDHLreg reg)     = "ld [hl], " ++ format reg
    show (LDHLimm val)     = "ld [hl]," ++ lowercase (hexify val)
    show (LDregHL reg)     = "ld " ++ format reg ++ ", [hl]"
    
hexify :: PrintfArg a => a -> String
hexify = printf "$%X"

format :: Show a => a -> String
format = lowercase . show

lowercase :: String -> String
lowercase = map toLower