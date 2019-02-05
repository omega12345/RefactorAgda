This tool has been tested with ghc version 8.4.3

<h1>Installation instructions</h1>
To be able to try the Atom package, you should either put the downloaded folder itself into ~/.atom/packages or put a folder alias there. Go into the downloaded folder in the terminal and run:


make

cabal install --enable-tests

The makefile contains instructions for compiling the Agda files. To execute it, you will need Agda 2.5.4. It will end with an error in a Haskell module, but that's okay.

<h1>RefactorAgda</h1>

This project is a refactoring tool for a subset of Agda called Baby-Agda. This subset is intended to be extended gradually to include the entire Agda language. Currently, it can be accessed either via a very sketchy Atom package or via cabal's test functionality. Hint: If the Atom package behaves strangely, look at the console in the developers' tools.

<h1>Currently supported refactorings</h1>

Rename (ctrl-k ctrl-r)

Push (reorder) function argument (ctrl-k ctrl-p)

Extract function (ctrl-k ctrl-e)

Toggle explicitness of an argument (ctrl-k ctrl-t)

Re-case-split (ctrl-k ctrl-c)  (fix errors of the type "incomplete pattern matching")

<h1> Missing a refactoring? </h1>
Feel free to open an issue if a desired refactoring is not on this list.
