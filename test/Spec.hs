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

{-# LANGUAGE OverloadedStrings #-}

import           Data.Word
import           Lazyboy
import           Lazyboy.Target.ASM 
import           Test.Hspec
import           Control.Monad.Trans.Except
import           Data.Either 

{-# ANN module ("HLint: ignore" :: String) #-}

disallow :: Except a b -> Expectation
disallow = (`shouldBe` True) . isLeft . runExcept 

instance Show Instruction

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
              let program = runExcept $ mapM compile $ execLazyboy disableLCD
              program `shouldBe` Right ["ld HL, $ff40", "ld [HL], 0"]
        describe "setLCDControl" $ do
            it "sets the lcd status" $ do
                let program = runExcept $ mapM compile $ execLazyboy $ setLCDControl $ defaultLCDControl { lcdBackgroundEnable = True }
                program `shouldBe` Right ["ld HL, $ff40", "ld [HL], 1"]
        describe "byte" $ do
            it "writes a byte into a register" $ do
                let program = runExcept $ mapM compile $ execLazyboy $ byte A 90
                program `shouldBe` Right ["ld A, 90"] 
        describe "setBackgroundPalette" $ do
            it "sets the background palette" $ do
                let def = runExcept $ mapM compile $ execLazyboy $ setBackgroundPalette defaultPalette
                let alt = runExcept $ mapM compile $ execLazyboy $ setBackgroundPalette $ BackgroundPalette White Light Dark Black
                def `shouldBe` Right ["ld HL, $ff47", "ld [HL], 228"]
                alt `shouldBe` Right ["ld HL, $ff47", "ld [HL], 27"]
        describe "onVblank" $ do
            it "waits for vblank before calling some code" $ do
                let program = runExcept $ mapM compile $ execLazyboy $ onVblank $ return ()
                program `shouldBe` Right [".L1:", "ld A, [$ff44]", "cp A, 145", "jr nz, .L1"]
                

    describe "Lazyboy.Types.execLazyboy" $ do
        it "compiles nested sequences in order" $ do
            let program = execLazyboy $ do
                    write (Address 0x2000) 0x97
                    write (Address 0x1000) 0x98
            program `shouldBe` [LDrrnn HL (Address 0x2000), LDHLn 0x97, LDrrnn HL (Address 0x1000), LDHLn 0x98]

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
                let program = runExcept $ mapM compile $ execLazyboy $ do
                        withLabel $ const $
                            write (Address 0xC000) 0x97
                program `shouldBe` Right [ "L1:"
                                   , "ld HL, $c000"
                                   , "ld [HL], 151"
                                   ]
        describe "withLocalLabel" $ do
            it "creates an appropriately formatted local label" $ do
                let program = runExcept $ mapM compile $ execLazyboy $ do
                        withLocalLabel $ const $
                            write (Address 0xC000) 0x97
                program `shouldBe` Right [ ".L1:"
                                   , "ld HL, $c000"
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
                let ab = runExcept $ mapM compile $ execLazyboy $ A `equalTo` B
                let bc = runExcept $ mapM compile $ execLazyboy $ B `equalTo` C
                let an = runExcept $ mapM compile $ execLazyboy $ A `equalTo` (5 :: Word8)
                let nc = runExcept $ mapM compile $ execLazyboy $ (100 :: Word8) `equalTo` C
                ab `shouldBe` Right ["cp A, B"]
                bc `shouldBe` Right ["ld A, B", "cp A, C"]
                an `shouldBe` Right ["cp A, 5"]
                nc `shouldBe` Right ["ld A, C", "cp A, 100"]
        describe "notEqualTo" $ do
            it "checks inequality between two values" $ do
                let ab = runExcept $ mapM compile $ execLazyboy $ A `notEqualTo` B
                let bc = runExcept $ mapM compile $ execLazyboy $ B `notEqualTo` C
                let an = runExcept $ mapM compile $ execLazyboy $ A `notEqualTo` (5 :: Word8)
                let nc = runExcept $ mapM compile $ execLazyboy $ (100 :: Word8) `notEqualTo` C
                ab `shouldBe` Right ["cp A, B"]
                bc `shouldBe` Right ["ld A, B", "cp A, C"]
                an `shouldBe` Right ["cp A, 5"]
                nc `shouldBe` Right ["ld A, C", "cp A, 100"]
        describe "greaterThan" $ do
            it "checks greater of two values" $ do
                let ab = runExcept $ mapM compile $ execLazyboy $ A `greaterThan` B
                let bc = runExcept $ mapM compile $ execLazyboy $ B `greaterThan` C
                let an = runExcept $ mapM compile $ execLazyboy $ A `greaterThan` (5 :: Word8)
                let nc = runExcept $ mapM compile $ execLazyboy $ (100 :: Word8) `greaterThan` C
                ab `shouldBe` Right ["cp A, B"]
                bc `shouldBe` Right ["ld A, B", "cp A, C"]
                an `shouldBe` Right ["cp A, 5"]
                nc `shouldBe` Right ["ld A, C", "cp A, 100"]
        describe "lessThan" $ do
            it "checks lesser of two values" $ do
                let ab = runExcept $ mapM compile $ execLazyboy $ A `lessThan` B
                let bc = runExcept $ mapM compile $ execLazyboy $ B `lessThan` C
                let an = runExcept $ mapM compile $ execLazyboy $ A `lessThan` (5 :: Word8)
                let nc = runExcept $ mapM compile $ execLazyboy $ (100 :: Word8) `lessThan` C
                ab `shouldBe` Right ["cp A, B"]
                bc `shouldBe` Right ["ld A, B", "cp A, C"]
                an `shouldBe` Right ["cp A, 5"]
                nc `shouldBe` Right ["ld A, C", "cp A, 100"]
        describe "if'" $ do
            it "provides conditional execution for more complex conditions" $ do
                let program = runExcept $ mapM compile $ execLazyboy $ if' (A `lessThan` B) $ return ()
                program `shouldBe` Right ["cp A, B", "jr c, .L1", ".L1:"]
        describe "and" $ do
            it "implements boolean AND for conditionals" $ do
                let program = runExcept $ mapM compile $ execLazyboy $ if' ((B `greaterThan` C) `Lazyboy.and` (A `equalTo` B)) $ return ()
                program `shouldBe` Right [ "ld A, B"
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
                let program = runExcept $ mapM compile $ execLazyboy $ if' ((C `greaterThan` (5 :: Word8)) `Lazyboy.or` (A `equalTo` C)) $ return ()
                program `shouldBe` Right [ "ld A, C"
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
                let program = runExcept $ mapM compile $ execLazyboy $ while (A `Lazyboy.notEqualTo` (55 :: Word8)) $ write (Address 0x0000) 0xA
                program `shouldBe` Right [ ".L1:"
                                   , "cp A, 55"
                                   , "jr nz, .L3"
                                   , "jr .L2"
                                   , ".L3:"
                                   , "ld HL, $0"
                                   , "ld [HL], 10"
                                   , "jr .L1"
                                   , ".L2:" ]

    describe "Lazyboy.Target.ASM" $ do
        describe "compile" $ do
            it "disallows loading [AF] into A" $ do
                disallow (compile $ LDArr AF)
            it "disallows loading [SP] into A" $ do
                disallow (compile $ LDArr SP)
            it "disallows loading [PC] into A" $ do
                disallow (compile $ LDArr PC)
            it "disallows loading A into [AF]" $ do
                disallow (compile $ LDrrA AF)
            it "disallows loading A into [SP]" $ do
                disallow (compile $ LDrrA SP)
            it "disallows loading A into [PC]" $ do
                disallow (compile $ LDrrA PC)
            it "disallows loading a 16 bit value into AF" $ do
                disallow $ compile $ LDrrnn AF $ Address 0x00
            it "disallows loading a 16 bit value into PC" $ do
                disallow $ compile $ LDrrnn PC $ Address 0x00
            it "disallows pushing stack pointer" $ do
                disallow (compile $ PUSH SP)
            it "disallows pushing program counter" $ do
                disallow (compile $ PUSH PC)
            it "disallows popping stack pointer" $ do
                disallow (compile $ POP SP)
            it "disallows popping program counter" $ do
                disallow (compile $ POP PC)
            it "disallows an invalid RST vector value" $ do
                disallow (compile $ RST 0x02)
            it "disallows adding AF to HL" $ do
                disallow (compile $ ADDHLrr AF)
            it "disallows adding PC to HL" $ do
                disallow (compile $ ADDHLrr PC)
            it "disallows incrementing AF" $ do
                disallow (compile $ INCrr AF)
            it "disallows incrementing PC" $ do
                disallow (compile $ INCrr PC)
            it "disallows decrementing AF" $ do
                disallow (compile $ DECrr AF)
            it "disallows decrementing PC" $ do
                disallow (compile $ DECrr PC)
            it "enforces only 3-bit values can be passed to BIT instructions" $ do
                disallow (compile $ BITnr 0x80 A)
            it "formats embedded byte sequences correctly" $ do
                let program = runExcept $ mapM compile $ execLazyboy $ tell [BYTES [97, 98]]
                program `shouldBe` Right ["db $61,$62" ]
            it "formats all other instructions correctly" $ do
                (runExcept $ compile (LDrr A B)) `shouldBe` Right "ld A, B"
                (runExcept $ compile (LDrn C 5)) `shouldBe` Right "ld C, 5"
                (runExcept $ compile (LDrHL A)) `shouldBe` Right "ld A, [HL]"
                (runExcept $ compile (LDHLr B)) `shouldBe` Right "ld [HL], B"
                (runExcept $ compile (LDHLn 1)) `shouldBe` Right "ld [HL], 1"
                (runExcept $ compile (LDArr BC)) `shouldBe` Right "ld A, [BC]"
                (runExcept $ compile (LDArr DE)) `shouldBe` Right "ld A, [DE]"
                (runExcept $ compile (LDArr HL)) `shouldBe` Right "ld A, [HL]"
                (runExcept $ compile (LDrrA BC)) `shouldBe` Right "ld [BC], A"
                (runExcept $ compile (LDrrA DE)) `shouldBe` Right "ld [DE], A"
                (runExcept $ compile (LDrrA HL)) `shouldBe` Right "ld [HL], A"
                (runExcept $ compile (LDAnn (Address 55))) `shouldBe` Right "ld A, [$37]" 
                (runExcept $ compile (LDnnA (Address 55))) `shouldBe` Right "ld [$37], A"
                (runExcept $ compile (LDAIO 0)) `shouldBe` Right "ldh A, [$FF00+$0]"
                (runExcept $ compile (LDIOA 1)) `shouldBe` Right "ldh [$FF00+$1], A"
                (runExcept $ compile (LDAIOC)) `shouldBe` Right "ldh A, [$FF00+C]"
                (runExcept $ compile (LDIOCA)) `shouldBe` Right "ldh [$FF00+C], A"
                (runExcept $ compile (LDHLAI)) `shouldBe` Right "ld [HL+], A"
                (runExcept $ compile (LDAHLI)) `shouldBe` Right "ld A, [HL+]"
                (runExcept $ compile (LDrrnn BC (Address 7))) `shouldBe` Right "ld BC, $7"
                (runExcept $ compile (LDSPHL)) `shouldBe` Right "ld SP, HL"
                (runExcept $ compile (PUSH BC)) `shouldBe` Right "PUSH BC"
                (runExcept $ compile (POP HL)) `shouldBe` Right "POP HL"
                (runExcept $ compile (JP (Address 43))) `shouldBe` Right "jp $2b"
                (runExcept $ compile (JP (Name (Global 1)))) `shouldBe` Right "jp L1"
                (runExcept $ compile (JP (Name (Local 40)))) `shouldBe` Right "jr .L40"
                (runExcept $ compile (JPHL)) `shouldBe` Right "jp HL"
                (runExcept $ compile (JPif Zero (Address 100))) `shouldBe` Right "jp z, $64"
                (runExcept $ compile (JPif NoCarry (Name (Global 20)))) `shouldBe` Right "jp nc, L20"
                (runExcept $ compile (JPif NonZero (Name (Local 4)))) `shouldBe` Right "jr nz, .L4"
                (runExcept $ compile (CALL (Address 50))) `shouldBe` Right "call $32"
                (runExcept $ compile (CALLif Zero (Address 50))) `shouldBe` Right "call z, $32"
                (runExcept $ compile (RET)) `shouldBe` Right "ret"
                (runExcept $ compile (RETif NonZero)) `shouldBe` Right "ret nz"
                (runExcept $ compile (RETi)) `shouldBe` Right "reti"
                (runExcept $ compile (ADDAr C)) `shouldBe` Right "add A, C"
                (runExcept $ compile (ADDAn 25)) `shouldBe` Right "add A, 25"
                (runExcept $ compile (ADDHL)) `shouldBe` Right "add A, [HL]"
                (runExcept $ compile (ADCAr L)) `shouldBe` Right "adc A, L"
                (runExcept $ compile (ADCAn 4)) `shouldBe` Right "adc A, 4"
                (runExcept $ compile (ADCHL)) `shouldBe` Right "adc A, [HL]"
                (runExcept $ compile (SUBAr A)) `shouldBe` Right "sub A, A"
                (runExcept $ compile (SUBAn 9)) `shouldBe` Right "sub A, 9"
                (runExcept $ compile (SUBHL)) `shouldBe` Right "sub A, [HL]"
                (runExcept $ compile (SBCAr B)) `shouldBe` Right "sbc A, B"
                (runExcept $ compile (SBCAn 3)) `shouldBe` Right "sbc A, 3"
                (runExcept $ compile (SBCAHL)) `shouldBe` Right "sbc A, [HL]"
                (runExcept $ compile (ANDr C)) `shouldBe` Right "and A, C"
                (runExcept $ compile (ANDn 1)) `shouldBe` Right "and A, 1"
                (runExcept $ compile (ANDHL)) `shouldBe` Right "and A, [HL]"
                (runExcept $ compile (XORr A)) `shouldBe` Right "xor A, A"
                (runExcept $ compile (XORn 1)) `shouldBe` Right "xor A, 1"
                (runExcept $ compile (XORHL)) `shouldBe` Right "xor A, [HL]"
                (runExcept $ compile (ORr C)) `shouldBe` Right "or A, C"
                (runExcept $ compile (ORn 10)) `shouldBe` Right "or A, 10"
                (runExcept $ compile (ORHL)) `shouldBe` Right "or A, [HL]"
                (runExcept $ compile (CPr B)) `shouldBe` Right "cp A, B"
                (runExcept $ compile (CPn 9)) `shouldBe` Right "cp A, 9"
                (runExcept $ compile (CPHL)) `shouldBe` Right "cp A, [HL]"
                (runExcept $ compile (INCr A)) `shouldBe` Right "inc A" 
                (runExcept $ compile (INCHL)) `shouldBe` Right "inc [HL]"
                (runExcept $ compile (DECr C)) `shouldBe` Right "dec C"
                (runExcept $ compile (DECHL)) `shouldBe` Right "dec [HL]"
                (runExcept $ compile (DAA)) `shouldBe` Right "daa"
                (runExcept $ compile (CPL)) `shouldBe` Right "cpl"
                (runExcept $ compile (ADDHLrr BC)) `shouldBe` Right "add HL, BC"
                (runExcept $ compile (ADDHLrr DE)) `shouldBe` Right "add HL, DE"
                (runExcept $ compile (ADDHLrr HL)) `shouldBe` Right "add HL, HL"
                (runExcept $ compile (ADDHLrr SP)) `shouldBe` Right "add HL, SP"
                (runExcept $ compile (INCrr BC)) `shouldBe` Right "inc BC"
                (runExcept $ compile (INCrr DE)) `shouldBe` Right "inc DE"
                (runExcept $ compile (INCrr HL)) `shouldBe` Right "inc HL"
                (runExcept $ compile (INCrr SP)) `shouldBe` Right "inc SP"
                (runExcept $ compile (DECrr BC)) `shouldBe` Right "dec BC"
                (runExcept $ compile (DECrr DE)) `shouldBe` Right "dec DE"
                (runExcept $ compile (DECrr HL)) `shouldBe` Right "dec HL"
                (runExcept $ compile (DECrr SP)) `shouldBe` Right "dec SP"
                (runExcept $ compile (RLCA)) `shouldBe` Right "rlca"
                (runExcept $ compile (RLA)) `shouldBe` Right "rla"
                (runExcept $ compile (RRCA)) `shouldBe` Right "rrca"
                (runExcept $ compile (RRA)) `shouldBe` Right "rra"
                (runExcept $ compile (RLC A)) `shouldBe` Right "rlc A"
                (runExcept $ compile (RLCHL)) `shouldBe` Right "rlc [HL]"
                (runExcept $ compile (RL C)) `shouldBe` Right "rl C"
                (runExcept $ compile (RLHL)) `shouldBe` Right "rl [HL]"
                (runExcept $ compile (RRC A)) `shouldBe` Right "rrc A"
                (runExcept $ compile (RRCHL)) `shouldBe` Right "rrc [HL]"
                (runExcept $ compile (RR B)) `shouldBe` Right "rr B"
                (runExcept $ compile (RRHL)) `shouldBe` Right "rr [HL]"
                (runExcept $ compile (SLA B)) `shouldBe` Right "sla B"
                (runExcept $ compile (SLAHL)) `shouldBe` Right "sla [HL]"
                (runExcept $ compile (SWAP B)) `shouldBe` Right "swap B"
                (runExcept $ compile (SWAPHL)) `shouldBe` Right "swap [HL]"
                (runExcept $ compile (SRA B)) `shouldBe` Right "sra B"
                (runExcept $ compile (SRAHL)) `shouldBe` Right "sra [HL]"
                (runExcept $ compile (SRL B)) `shouldBe` Right "srl B"
                (runExcept $ compile (SRLHL)) `shouldBe` Right "srl [HL]"
                (runExcept $ compile (CCF)) `shouldBe` Right "ccf"
                (runExcept $ compile (SCF)) `shouldBe` Right "scf"
                (runExcept $ compile (NOP)) `shouldBe` Right "nop"
                (runExcept $ compile (HALT)) `shouldBe` Right "halt"
                (runExcept $ compile (STOP)) `shouldBe` Right "stop"
                (runExcept $ compile (DI)) `shouldBe` Right "di"
                (runExcept $ compile (EI)) `shouldBe` Right "ei"

                -- these fail
                disallow $ compile $ LDArr AF
                disallow $ compile $ LDrrA AF
                disallow $ compile $ LDrrnn AF $ Address 0
                disallow $ compile $ LDrrnn PC $ Name $ Local 1
                disallow $ compile $ PUSH SP
                disallow $ compile $ PUSH PC
                disallow $ compile $ POP SP
                disallow $ compile $ POP PC
                disallow $ compile $ ADDHLrr AF
                disallow $ compile $ INCrr AF
                disallow $ compile $ DECrr AF

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
