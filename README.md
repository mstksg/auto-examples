auto-examples
=============

Various examples to demonstrate features of the in-development [auto][]
library, and also as guides for writing your own applications.  API subject to
much change.

[auto]: https://github.com/mstksg/auto

As things are still in development, here is the best way to install and run
these:

```bash
# clone the in-development branch of the auto library
$ git clone https://github.com/mstksg/auto -b develop

# clone this examples repository
$ git clone https://github.com/mstksg/auto-examples
$ cd auto-examples

# set up the sandbox, pointing to the library source on-disk
$ cabal sandbox init
$ cabal sandbox add-source ../auto

# install
$ cabal install
```

And the executables should all be in `./.cabal-sandbox/bin` of the
`auto-examples` dir.

Examples
--------

### Hangman

A fully featured command-line hangman game.  Made to demonstrate many
high-level features, like the composition of locally stateful autos with
proc-do notation, implicit serializability, switching, and usage of
`interact`.  Lays out some pretty common idioms and displays some design
methodology.

Note the lack of a global "hangman state".  All the components of the state
--- the current word, the wrong guesses, the player scores, etc. --- are
isolated from eachother and only interact when needed.  The `Puzzle` type only
contains information for the console to display the current "output" of the
puzzle --- it doesn't even contain the solution.

Also, note the principled reading and saving of the game auto using `readAuto`
and `writeAuto`.

Admittedly it's a lot "longer" in terms of lines of code than the simple
explicit-state-passing version (even without the gratuitous whitespace and
commenting).  Part of this is because Hangman is pretty simple.  But I really
feel like the whole thing "reads" well, and is in a more understandable
high-level declarative/denotative style than such an approach.

### Logger

Mostly used to demonstrate "automatic serialization".  Using the `serializing`
combinator, we transform a normal auto representing a logging process into an
auto that automatically, implicitly, and constantly serializes itself...and
automatically re-loads the saved state on the program initialization.


