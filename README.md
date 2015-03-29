sudoku
======

A little sudoku solver in Erlang.  This is sort of a port of my
[sudoku solver](https://github.com/tommay/sudoku) done in various
other languages (Ruby, JavaScript, CoffeeScript) but Erlang is a
different beast entirely even for a Lisp hacker.  I've been learning a
lot about how to do functional programming somewhat cleanly in Erlang
with a module per "object", tagged values, etc.  Functional
programming seems to demand a "tell, don't ask" style and violating
the law of Demeter is right out.  So it should be a good influence on
my designs in other languages.  At least I've been a fan of
immutability for decades

This finally works!  I made an escript executable that takes a filename
and prints any solutions.

I've gotten quite comfortable with the pattern matching and like it a
lot.  If we get this, do that.  If we get something else, do the other
thing.  Give this piece of the arguments a name so we can use it
later.

It's not clear whether I'll be sticking with some of my design
approaches.  It's also strictly functional with no actors or
concurrency.  I'll play with those later, probably when I add
statistics of the various techniques.

Update: I wanted multiple processes when solving puzzles with multiple
solutions so I could use all available cores.  But that made an
overwhelming number of processes for Erlang to deal with and it would
run out of memory and crash.  So now there is a limiter process which
tracks the number of spawned processes and doesn't allow more than N
to run at once.  There is also a stats server process.

Somtimes it still feels weird to be writing functions that "don't do
anything"; they just look at their arguments and return some new junk,
often a slightly modified copy of itself.  It's like telling a dog to
walk across the room, but instead of actually walking across the room
it creates a new dog that's on the other side of the room and you use
that dog instead.  And you still have the old dog on your side of the
room, until you ignore it long enough and the garbage collector cleans
it up.
