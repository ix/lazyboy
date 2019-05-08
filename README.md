![LAZYBOY](meta/logo.png)
[![Build Status](https://travis-ci.org/ix/lazyboy.svg?branch=master)](https://travis-ci.org/ix/lazyboy)
[![Coverage Status](https://coveralls.io/repos/github/ix/lazyboy/badge.svg?branch=master)](https://coveralls.io/github/ix/lazyboy?branch=master)
---

An embedded domain-specific language + compiler written in Haskell for producing code to run on the Nintendo Game Boy.

Also features a library for manipulating constructs such as memory and graphics.

Currently, RGBASM is the only output target, but in the future native machine code generation is planned.

Syntax example (will be updated as more complex constructs are added):
```haskell
main :: IO ()
main = rom >>= T.putStrLn
    where rom = compileROM $ do
            smiley <- embedBytes image
            memcpy (Name smiley) (Address vram) $ fromIntegral $ length image
            setBackgroundPalette defaultPalette
            setLCDControl $ defaultLCDControl { lcdDisplayEnable = True, lcdBackgroundEnable = True }
            freeze

          image :: [Word8]
          image = [0x00, 0x00, 0x00, 0x00, 0x24, 0x24, 0x00, 0x00, 0x81, 0x81, 0x7e, 0x7e, 0x00, 0x00, 0x00, 0x00]
```

See `app/Main.hs` for a full usage example.

Build a ROM (output will be named `main.gb`):
```
stack run > examples/main.asm
cd examples && make NAME=main
```

# Special thanks
Thanks to [Franceso149](https://github.com/Francesco149) and [Bonzi](https://github.com/bnzis) for kindly devoting their time and knowledge to the project. The graphics functionality in particular would not be where it is without their assistance.