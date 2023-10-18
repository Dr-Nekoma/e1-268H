# dcpu16-emulator-hs

Implementation of Notch's [dcpu-16](https://web.archive.org/web/20120509184912/http://0x10c.com/doc/dcpu-16.txt) CPU in Haskell.

## Status

- [x] Basic infrastructure of the emulator
- [ ] Command line interface
- [ ] Test suite
- [ ] Assembler
- [ ] Debugger interface (tui, breakpoints, inspect/change memory and regisgters, disassemble, etc)
- [ ] Peripherals (video, keyboard, serial interface, etc)

## Building

```shell
stack build
``` 

## Running

There's no user interface yet, so fire up GHCi...

``` shell
stack repl
```

...and load the example:

```shell
ghci> import qualified Data.ByteString as B
ghci> import Control.Monad.Trans
ghci> import Control.Monad

ghci> readProgram = lift . B.readFile $ "examples/32-bit-add.hex"

ghci> runIOComputer (readProgram >>= loadProgram >> replicateM_ 5 execInstruction >> dumpMachine >>= lift . putStr) newIOComputer
```
