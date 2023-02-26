# brainhuck

A Haskell implementation of a Brainfuck interpreter.

The interpeter is implemented as an executable (installed with `stack install` in the directory), which can take 
the path of Brainfuck source code, or using the `--stdin` flag, execute code given as an argument to the 
interpeter.

Usage examples:

```bash
brainhuck path/to/program.b

brainhuck --stdin ">>>>+"

brainhuck program.b -s 100 > debug.txt
```

Flags/Options:   

```bash
-s --size               # memory size (default: 500 cells)
--stdin                 # execute code given as an argument
## -d --debug            # debug mode UNAVAILABLE AT THE MOMENT may return in the future 
```

I have yet to to document the source code at least to some extent, nevertheless I think it's fairly readable, 
though maybe not so idiomatic, since I'm still a Haskell beginner-intermediate.

This interpreter allows for different datatypes to be used as the memory used, only requiring for each datatype
a couple of typeclasses, and few more code. 

