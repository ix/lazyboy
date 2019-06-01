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
