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

### hangman

A fully featured command-line hangman game.  Made to demonstrate many
high-level features, like the composition of locally stateful autos with
proc-do notation, implicit serializability, switching, and usage of
`interact`.  Lays out some pretty common idioms and displays some design
methodology.

Note the lack of a global "hangman state".  All the components of the state
--- the current word, the wrong guesses, the player scores, etc. --- are
isolated from each other and only interact when needed.  The `Puzzle` type only
contains information for the console to display the current "output" of the
puzzle --- it doesn't even contain the solution.

Also, note the principled reading and saving of the game auto using `readAuto`
and `writeAuto`.

Admittedly it's a lot "longer" in terms of lines of code than the simple
explicit-state-passing version (even without the gratuitous whitespace and
commenting).  Part of this is because Hangman is pretty simple.  But I really
feel like the whole thing "reads" well, and is in a more understandable
high-level declarative/denotative style than such an approach.

### logger

Mostly used to demonstrate "automatic serialization".  Using the `serializing`
combinator, we transform a normal auto representing a logging process into an
auto that automatically, implicitly, and constantly serializes itself...and
automatically re-loads the saved state on the program initialization.

### chatbot

Lots of concepts demonstrated here.  In fact, this was one of the motivating
reasons for the entire *auto* library in the first place.

First, a "real world" interface; the Auto is operated and run over an IRC
server using the [simpleirc][] library.  The library waits on messages, runs
the Auto, sends out the outputs, and stores the new Auto.

[simpleirc]: http://hackage.haskell.org/package/simpleirc

Secondly, the "monoidal" nature of Auto is taken full advantage of here. Each
individual bot module is a full fledged bot (of type `ChatBot m`, or `ChatBot'
m`).  The "final" bot is the `mconcat`/monoid sum of individual modules.  The
monoid nature means that pairs of bots can be combined and modified together
and combined with other bots, etc.

Like legos! :D

Third --- there is no "global chatbot state".  That is, *every module*
maintains *its own internal state*, isolated and unrelated to the other
modules.  In the "giant state monad" approach, *even with* using zoom and
stuff from lens...every time you add a stateful module, you *have to change
the global state type*.  That is, you'd have to "fit in" the room for the new
state in your global state type.

In this way, adding a module is as simple as just adding another `(<>)` or
item to the `mconcat` list.  Each module is completely self-contained and
maintains its own state; adding a module does not affect a single aspect of
any other part of the code base.

Fourth, serializing individual components of wires "automatically".  We don't
serialize the entire chatbot; we can simply serialize individual Auto
components in the chain.  This is because of the type of `serializing' fp`:

```haskell
serializing' fp :: MonadIO => Auto m a b -> Auto m a b
```

It basically takes an Auto and returns a Auto that is identical in every
way...except self-reloading and self-serializing.  Whenever you use that
transformed Auto as a component of any other one, that individual component
will be self-reloading and self-serializing, even if it's embedded deep in a
complex composition.

```haskell
f (serializing' fp a1) (serializing' fp a2)
= serializing' fp (f a1 a2)
```

Also, there is the demonstration of using "lifters", like `perRoom` to
transform a `ChatBot` who can only send messages back to the channel it
received messages to a `ChatBot` who can send messages to any channel.  They
behave the same way --- but now they can be combined with other such bots.  In
this way, you can write "limited bots", and still have them "play well" and
combine with other bots --- inspired by the principles of Gabriel Gonzalez's
[Functor Design Pattern][fdp].

[fdp]: http://www.haskellforall.com/2012/09/the-functor-design-pattern.html

The individual bots themselves all demonstrate usage of common Auto
combinators, like `mkAccum` (which modifies the state with input continually,
with the given function) --- also much usage of the `Blip` mechanism and
semantics --- much of the bots respond to "blips" --- like detected user
commands, and the day changing.

Working with streams of blips, "scanning over them" (like `mkAccum` but with
blips), and consolidating blip streams back into normal streams are all
demonstrated.

### life

[Conway's Game of Life][cgol] implementation.  Demonstration of
non-interactive automation/simulation/cellular automaton.  In the technical
aspects, a demonstration of the `rec`/`ArrowLoop` mechanisms for recursive,
graph-like Auto connections.

I consider this to be another compelling demonstration of the power of
denotative style.  The entire "game logic" is 3-4 lines long, depending on how
you cut it (not including helper functions) and (as I would hope) is very
readable.  Some nice practice with the various `Blip` combinators, as well!

[cgol]: http://en.wikipedia.org/wiki/Conway's_Game_of_Life


