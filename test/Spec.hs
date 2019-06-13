{-|
    Module      : Main (Test)
    Description : Test suite for Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    A test suite for Lazyboy.
-}

import           Control.Exception  (evaluate)
import           Data.Word
import           Lazyboy
import           Lazyboy.Target.ASM
import           Test.Hspec

disallow cmd = evaluate cmd `shouldThrow` anyException

main :: IO ()
main = hspec $ do
    describe "Lazyboy.IO" $ do
        context "when asked to pack an LCDControl" $ do
            it "packs an LCDControl of all False values to 0" $ do
                pack defaultLCDControl `shouldBe` 0
            it "packs an LCDControl with background and display enabled to 129" $ do
                pack (LCDControl True False False False False False False True) `shouldBe` 129
            it "packs other LCDControls correctly" $ do
                pack (defaultLCDControl { lcdEnableObjects = True }) `shouldBe` 2
        context "when asked to pack a BackgroundPalette" $ do
            it "packs a BackgroundPalette of all-black to 255" $ do
                pack (BackgroundPalette Black Black Black Black) `shouldBe` 255
            it "packs a BackgroundPalette of all-white to 0" $ do
                pack (BackgroundPalette White White White White) `shouldBe` 0
            it "correctly packs other BackgroundPalettes" $ do
                pack (BackgroundPalette White Light White White) `shouldBe` 16
        describe "disableLCD" $ do 
            it "turns off the lcd" $ do
                let program = map show $ execLazyboy disableLCD
                program `shouldBe` ["ld HL, $FF40", "ld [HL], 0"]
        describe "setLCDControl" $ do
            it "sets the lcd status" $ do
                let program = map show $ execLazyboy $ setLCDControl $ defaultLCDControl { lcdBackgroundEnable = True }
                program `shouldBe` ["ld HL, $FF40", "ld [HL], 1"]
        describe "byte" $ do
            it "writes a byte into a register" $ do
                let program = map show $ execLazyboy $ byte A 90
                program `shouldBe` ["ld A, 90"] 
        describe "setBackgroundPalette" $ do
            it "sets the background palette" $ do
                let def = map show $ execLazyboy $ setBackgroundPalette defaultPalette
                let alt = map show $ execLazyboy $ setBackgroundPalette $ BackgroundPalette White Light Dark Black
                def `shouldBe` ["ld HL, $FF47", "ld [HL], 228"]
                alt `shouldBe` ["ld HL, $FF47", "ld [HL], 27"]
        describe "onVblank" $ do
            it "waits for vblank before calling some code" $ do
                let program = map show $ execLazyboy $ onVblank $ return ()
                program `shouldBe` [".L1:", "ld A, [$FF44]", "cp A, 145", "jr nz, .L1"]
                

    describe "Lazyboy.Types.execLazyboy" $ do
        it "compiles nested sequences in order" $ do
            let sequence = execLazyboy $ do
                    write (Address 0x2000) 0x97
                    write (Address 0x1000) 0x98
            sequence `shouldBe` [LDrrnn HL (Address 0x2000), LDHLn 0x97, LDrrnn HL (Address 0x1000), LDHLn 0x98]

    describe "Lazyboy.Control" $ do
        describe "cond" $ do
            it "correctly implements conditionals" $ do
                let program = execLazyboy $ do
                        cond NonZero $ do
                            freeze
                program `shouldBe` [JPif NonZero $ Name $ Local 1, LABEL $ Global 2, DI, HALT, JP $ Name $ Global 2, LABEL $ Local 1]
            it "handles nested conditionals correctly" $ do
                let program = execLazyboy $ do
                        cond Zero $ do
                            cond NonZero $ do
                                freeze
                program `shouldBe` [ JPif Zero $ Name $ Local 1
                                , JPif NonZero $ Name $ Local 2
                                , LABEL $ Global 3
                                , DI
                                , HALT
                                , JP $ Name $ Global 3
                                , LABEL $ Local 2
                                , LABEL $ Local 1
                                ]
        describe "withLabel" $ do
            it "creates an appropriately formatted global label" $ do
                let program = map show $ execLazyboy $ do
                        withLabel $ \label -> do
                            write (Address 0xC000) 0x97
                program `shouldBe` [ "L1:"
                                   , "ld HL, $C000"
                                   , "ld [HL], 151"
                                   ]
        describe "withLocalLabel" $ do
            it "creates an appropriately formatted local label" $ do
                let program = map show $ execLazyboy $ do
                        withLocalLabel $ \label -> do
                            write (Address 0xC000) 0x97
                program `shouldBe` [ ".L1:"
                                   , "ld HL, $C000"
                                   , "ld [HL], 151"
                                   ]
        describe "embedImage" $ do
            it "leverages RGBASM to include a binary" $ do
                let program = execLazyboy $ embedImage "test.bin"
                program `shouldBe` [JP $ Name $ Global 2, LABEL $ Global 1, INCLUDE "test.bin", LABEL $ Global 2]
        describe "embedBytes" $ do
            it "defines a raw sequence of bytes" $ do
                let program = execLazyboy $ embedBytes [0x00, 0x01, 0x02]
                program `shouldBe` [JP $ Name $ Global 2, LABEL $ Global 1, BYTES [0x00, 0x01, 0x02], LABEL $ Global 2]
        describe "not" $ do
            it "inverts a given condition flag" $ do
                let flags = map ((\f -> fst $ evalRWS f () 1) . Lazyboy.not . return) [Zero, NonZero, Carry, NoCarry]
                flags `shouldBe` [NonZero, Zero, NoCarry, Carry]
        describe "equalTo" $ do
            it "checks equality between two values" $ do
                let ab = map show $ execLazyboy $ A `equalTo` B
                let bc = map show $ execLazyboy $ B `equalTo` C
                let an = map show $ execLazyboy $ A `equalTo` (5 :: Word8)
                let nc = map show $ execLazyboy $ (100 :: Word8) `equalTo` C
                ab `shouldBe` ["cp A, B"]
                bc `shouldBe` ["ld A, B", "cp A, C"]
                an `shouldBe` ["cp A, 5"]
                nc `shouldBe` ["ld A, C", "cp A, 100"]
        describe "notEqualTo" $ do
            it "checks inequality between two values" $ do
                let ab = map show $ execLazyboy $ A `notEqualTo` B
                let bc = map show $ execLazyboy $ B `notEqualTo` C
                let an = map show $ execLazyboy $ A `notEqualTo` (5 :: Word8)
                let nc = map show $ execLazyboy $ (100 :: Word8) `notEqualTo` C
                ab `shouldBe` ["cp A, B"]
                bc `shouldBe` ["ld A, B", "cp A, C"]
                an `shouldBe` ["cp A, 5"]
                nc `shouldBe` ["ld A, C", "cp A, 100"]
        describe "greaterThan" $ do
            it "checks greater of two values" $ do
                let ab = map show $ execLazyboy $ A `greaterThan` B
                let bc = map show $ execLazyboy $ B `greaterThan` C
                let an = map show $ execLazyboy $ A `greaterThan` (5 :: Word8)
                let nc = map show $ execLazyboy $ (100 :: Word8) `greaterThan` C
                ab `shouldBe` ["cp A, B"]
                bc `shouldBe` ["ld A, B", "cp A, C"]
                an `shouldBe` ["cp A, 5"]
                nc `shouldBe` ["ld A, C", "cp A, 100"]
        describe "lessThan" $ do
            it "checks lesser of two values" $ do
                let ab = map show $ execLazyboy $ A `lessThan` B
                let bc = map show $ execLazyboy $ B `lessThan` C
                let an = map show $ execLazyboy $ A `lessThan` (5 :: Word8)
                let nc = map show $ execLazyboy $ (100 :: Word8) `lessThan` C
                ab `shouldBe` ["cp A, B"]
                bc `shouldBe` ["ld A, B", "cp A, C"]
                an `shouldBe` ["cp A, 5"]
                nc `shouldBe` ["ld A, C", "cp A, 100"]
        describe "if'" $ do
            it "provides conditional execution for more complex conditions" $ do
                let program = map show $ execLazyboy $ if' (A `lessThan` B) $ return ()
                program `shouldBe` ["cp A, B", "jr c, .L1", ".L1:"]
        describe "and" $ do
            it "implements boolean AND for conditionals" $ do
                let program = map show $ execLazyboy $ if' ((B `greaterThan` C) `Lazyboy.and` (A `equalTo` B)) $ return ()
                program `shouldBe` [ "ld A, B"
                                   , "cp A, C"
                                   , "jr nc, .L1"
                                   , "ld L, 1" 
                                   , ".L1:"
                                   , "cp A, B" 
                                   , "jr nz, .L2"
                                   , "ld A, 1"
                                   , ".L2:"
                                   , "and A, L"
                                   , "jr z, .L3"
                                   , ".L3:" ]
        describe "or" $ do
            it "implements boolean OR for conditionals" $ do
                let program = map show $ execLazyboy $ if' ((C `greaterThan` (5 :: Word8)) `Lazyboy.or` (A `equalTo` C)) $ return ()
                program `shouldBe` [ "ld A, C"
                                   , "cp A, 5"
                                   , "jr nc, .L1"
                                   , "ld L, 1" 
                                   , ".L1:"
                                   , "cp A, C" 
                                   , "jr nz, .L2"
                                   , "ld A, 1"
                                   , ".L2:"
                                   , "or A, L"
                                   , "jr z, .L3"
                                   , ".L3:" ]
        describe "while" $ do
            it "implements an imperative WHILE loop with a condition" $ do
                let program = map show $ execLazyboy $ while (A `Lazyboy.notEqualTo` (55 :: Word8)) $ write (Address 0x0000) 0xA
                program `shouldBe` [ ".L1:"
                                   , "cp A, 55"
                                   , "jr nz, .L3"
                                   , "jr .L2"
                                   , ".L3:"
                                   , "ld HL, $0"
                                   , "ld [HL], 10"
                                   , "jr .L1"
                                   , ".L2:" ]

    describe "Lazyboy.Target.ASM" $ do
        describe "show" $ do
            it "disallows loading [AF] into A" $ do
                disallow (show $ LDArr AF)
            it "disallows loading [SP] into A" $ do
                disallow (show $ LDArr SP)
            it "disallows loading [PC] into A" $ do
                disallow (show $ LDArr PC)
            it "disallows loading A into [AF]" $ do
                disallow (show $ LDrrA AF)
            it "disallows loading A into [SP]" $ do
                disallow (show $ LDrrA SP)
            it "disallows loading A into [PC]" $ do
                disallow (show $ LDrrA PC)
            it "disallows loading a 16 bit value into AF" $ do
                disallow $ show $ LDrrnn AF $ Address 0x00
            it "disallows loading a 16 bit value into PC" $ do
                disallow $ show $ LDrrnn PC $ Address 0x00
            it "disallows pushing stack pointer" $ do
                disallow (show $ PUSH SP)
            it "disallows pushing program counter" $ do
                disallow (show $ PUSH PC)
            it "disallows popping stack pointer" $ do
                disallow (show $ POP SP)
            it "disallows popping program counter" $ do
                disallow (show $ POP PC)
            it "disallows an invalid RST vector value" $ do
                disallow (show $ RST 0x02)
            it "disallows adding AF to HL" $ do
                disallow (show $ ADDHLrr AF)
            it "disallows adding PC to HL" $ do
                disallow (show $ ADDHLrr PC)
            it "disallows incrementing AF" $ do
                disallow (show $ INCrr AF)
            it "disallows incrementing PC" $ do
                disallow (show $ INCrr PC)
            it "disallows decrementing AF" $ do
                disallow (show $ DECrr AF)
            it "disallows decrementing PC" $ do
                disallow (show $ DECrr PC)
            it "enforces only 3-bit values can be passed to BIT instructions" $ do
                disallow (show $ BITnr 0x80 A)
            it "formats embedded byte sequences correctly" $ do
                let program = map show $ execLazyboy $ tell [BYTES [97, 98]]
                program `shouldBe` ["db $61,$62" ]
            it "formats all other instructions correctly" $ do
                show (LDrr A B) `shouldBe` "ld A, B"
                show (LDrn C 5) `shouldBe` "ld C, 5"
                show (LDrHL A) `shouldBe` "ld A, [HL]"
                show (LDHLr B) `shouldBe` "ld [HL], B"
                show (LDHLn 1) `shouldBe` "ld [HL], 1"
                show (LDArr BC) `shouldBe` "ld A, [BC]"
                show (LDArr DE) `shouldBe` "ld A, [DE]"
                show (LDArr HL) `shouldBe` "ld A, [HL]"
                show (LDrrA BC) `shouldBe` "ld [BC], A"
                show (LDrrA DE) `shouldBe` "ld [DE], A"
                show (LDrrA HL) `shouldBe` "ld [HL], A"
                show (LDAnn (Address 55)) `shouldBe` "ld A, [$37]" 
                show (LDnnA (Address 55)) `shouldBe` "ld [$37], A"
                show (LDAIO 0) `shouldBe` "ldh A, [$FF00+$0]"
                show (LDIOA 1) `shouldBe` "ldh [$FF00+$1], A"
                show (LDAIOC) `shouldBe` "ldh A, [$FF00+C]"
                show (LDIOCA) `shouldBe` "ldh [$FF00+C], A"
                show (LDHLAI) `shouldBe` "ld [HL+], A"
                show (LDAHLI) `shouldBe` "ld A, [HL+]"
                show (LDrrnn BC (Address 7)) `shouldBe` "ld BC, $7"
                show (LDSPHL) `shouldBe` "ld SP, HL"
                show (PUSH BC) `shouldBe` "PUSH BC"
                show (POP HL) `shouldBe` "POP HL"
                show (JP (Address 43)) `shouldBe` "jp $2B"
                show (JP (Name (Global 1))) `shouldBe` "jp L1"
                show (JP (Name (Local 40))) `shouldBe` "jr .L40"
                show (JPHL) `shouldBe` "jp HL"
                show (JPif Zero (Address 100)) `shouldBe` "jp z, $64"
                show (JPif NoCarry (Name (Global 20))) `shouldBe` "jp nc, L20"
                show (JPif NonZero (Name (Local 4))) `shouldBe` "jr nz, .L4"
                show (CALL (Address 50)) `shouldBe` "call $32"
                show (CALLif Zero (Address 50)) `shouldBe` "call z, $32"
                show (RET) `shouldBe` "ret"
                show (RETif NonZero) `shouldBe` "ret nz"
                show (RETi) `shouldBe` "reti"
                show (ADDAr C) `shouldBe` "add A, C"
                show (ADDAn 25) `shouldBe` "add A, 25"
                show (ADDHL) `shouldBe` "add A, [HL]"
                show (ADCAr L) `shouldBe` "adc A, L"
                show (ADCAn 4) `shouldBe` "adc A, 4"
                show (ADCHL) `shouldBe` "adc A, [HL]"
                show (SUBAr A) `shouldBe` "sub A, A"
                show (SUBAn 9) `shouldBe` "sub A, 9"
                show (SUBHL) `shouldBe` "sub A, [HL]"
                show (SBCAr B) `shouldBe` "sbc A, B"
                show (SBCAn 3) `shouldBe` "sbc A, 3"
                show (SBCAHL) `shouldBe` "sbc A, [HL]"
                show (ANDr C) `shouldBe` "and A, C"
                show (ANDn 1) `shouldBe` "and A, 1"
                show (ANDHL) `shouldBe` "and A, [HL]"
                show (XORr A) `shouldBe` "xor A, A"
                show (XORn 1) `shouldBe` "xor A, 1"
                show (XORHL) `shouldBe` "xor A, [HL]"
                show (ORr C) `shouldBe` "or A, C"
                show (ORn 10) `shouldBe` "or A, 10"
                show (ORHL) `shouldBe` "or A, [HL]"
                show (CPr B) `shouldBe` "cp A, B"
                show (CPn 9) `shouldBe` "cp A, 9"
                show (CPHL) `shouldBe` "cp A, [HL]"
                show (INCr A) `shouldBe` "inc A" 
                show (INCHL) `shouldBe` "inc [HL]"
                show (DECr C) `shouldBe` "dec C"
                show (DECHL) `shouldBe` "dec [HL]"
                show (DAA) `shouldBe` "daa"
                show (CPL) `shouldBe` "cpl"
                show (ADDHLrr BC) `shouldBe` "add HL, BC"
                show (ADDHLrr DE) `shouldBe` "add HL, DE"
                show (ADDHLrr HL) `shouldBe` "add HL, HL"
                show (ADDHLrr SP) `shouldBe` "add HL, SP"
                show (INCrr BC) `shouldBe` "inc BC"
                show (INCrr DE) `shouldBe` "inc DE"
                show (INCrr HL) `shouldBe` "inc HL"
                show (INCrr SP) `shouldBe` "inc SP"
                show (DECrr BC) `shouldBe` "dec BC"
                show (DECrr DE) `shouldBe` "dec DE"
                show (DECrr HL) `shouldBe` "dec HL"
                show (DECrr SP) `shouldBe` "dec SP"
                show (RLCA) `shouldBe` "rlca"
                show (RLA) `shouldBe` "rla"
                show (RRCA) `shouldBe` "rrca"
                show (RRA) `shouldBe` "rra"
                show (RLC A) `shouldBe` "rlc A"
                show (RLCHL) `shouldBe` "rlc [HL]"
                show (RL C) `shouldBe` "rl C"
                show (RLHL) `shouldBe` "rl [HL]"
                show (RRC A) `shouldBe` "rrc A"
                show (RRCHL) `shouldBe` "rrc [HL]"
                show (RR B) `shouldBe` "rr B"
                show (RRHL) `shouldBe` "rr [HL]"
                show (SLA B) `shouldBe` "sla B"
                show (SLAHL) `shouldBe` "sla [HL]"
                show (SWAP B) `shouldBe` "swap B"
                show (SWAPHL) `shouldBe` "swap [HL]"
                show (SRA B) `shouldBe` "sra B"
                show (SRAHL) `shouldBe` "sra [HL]"
                show (SRL B) `shouldBe` "srl B"
                show (SRLHL) `shouldBe` "srl [HL]"
                show (CCF) `shouldBe` "ccf"
                show (SCF) `shouldBe` "scf"
                show (NOP) `shouldBe` "nop"
                show (HALT) `shouldBe` "halt"
                show (STOP) `shouldBe` "stop"
                show (DI) `shouldBe` "di"
                show (EI) `shouldBe` "ei"

                -- these throw
                disallow $ show $ LDArr AF
                disallow $ show $ LDrrA AF
                disallow $ show $ LDrrnn AF $ Address 0
                disallow $ show $ LDrrnn PC $ Name $ Local 1
                disallow $ show $ PUSH SP
                disallow $ show $ PUSH PC
                disallow $ show $ POP SP
                disallow $ show $ POP PC
                disallow $ show $ ADDHLrr AF
                disallow $ show $ INCrr AF
                disallow $ show $ DECrr AF

    describe "Lazyboy.Constants" $ do
        it "has valid constants" $ do
            wram0 `shouldBe` 0xC000
            wram1 `shouldBe` 0xD000
            joypad `shouldBe` 0xFF00
            lcdc `shouldBe` 0xFF40
            lcdstate `shouldBe` 0xFF41
            scx `shouldBe` 0xFF42
            scy `shouldBe` 0xFF43
            ly `shouldBe` 0xFF44
            lyc `shouldBe` 0xFF45
            dma `shouldBe` 0xFF46
            bgp `shouldBe` 0xFF47
            vram `shouldBe` 0x8000
            background1 `shouldBe` 0x9800
            background2 `shouldBe` 0x9C00
            hram `shouldBe` 0xFF80
            oam `shouldBe` 0xFE00
            screenWidth `shouldBe` 160
            screenHeight `shouldBe` 144