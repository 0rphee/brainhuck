# brainhuck

A Haskell implementation of a Brainfuck interpreter.

Though this interpreter can run a simple "Hello World!" program, it is not yet able to
handle nested loops properly. 

The interpeter is implemented as an executable, which can tke the path of Brainfuck
source code, or using the `--stdin` flag, execute code given as an argument to the 
interpeter.

Usage examples:

```
brainhuck path/to/program.b

brainhuck --stdin ">>>>+"

brainhuck program.b -d -s 100
```

Flags/Options:   

```
-s --size    memory size
-d --debug   debug mode
--stdin      execute code given as an argument

```

I'm a bit regretful of my decision for the initial approach of the main logic of
the interpeter, as I've encountered some hiccups while thinking through the recursion
implementation for loops. Nevertheless, I'm determined to solve it this way, with the 
hope of coming up with a better alternative after I get the initial approach working
properly. I imagine this could involve a proper AST and a different approach to loops,
for starters.

Though this project's README is yet to be more welcoming, I have attempted to document 
the source code to some extent, and I think it's fairly readable, though maybe not so
idiomatic, since I'm still a Haskell beginner-intermediate.




