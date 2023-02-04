# brainhuck

A Haskell implementation of a Brainfuck interpreter.

The interpeter is implemented as an executable, which can tke the path of Brainfuck
source code, or using the `--stdin` flag, execute code given as an argument to the 
interpeter.

Usage examples:

```
brainhuck path/to/program.b

brainhuck --stdin ">>>>+"

brainhuck program.b -d -s 100 > debug.txt
```

Flags/Options:   

```
-s --size               memory size
-d --debug              debug mode
--stdin                 execute code given as an argument
```

Though this project's README is yet to be more welcoming, I have attempted to document 
the source code to some extent, and I think it's fairly readable, though maybe not so
idiomatic, since I'm still a Haskell beginner-intermediate.

Currently, I'm in the process of writing a new interface for a "generic?" implementation
of the interpreter, in order to be able to write the functionality of the interpreter
from scratch with multiple approaches. (Making it easy to use different data types for
the memory, etc.)
