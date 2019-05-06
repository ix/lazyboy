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
            write wram1 0xC0
            write (wram1 + 1) 0xDE
            write (wram0 + 10) 0xFA
            write (wram0 + 11) 0xCE
            memcpy wram1 wram0 10
            cond Zero $ do
                memcpy (wram0 + 10) (wram1 + 10) 10
            freeze

```

See `app/Main.hs` for a full usage example.

Build a ROM (output will be named `main.gb`):
```
stack run > examples/main.asm
cd examples && make NAME=main
```