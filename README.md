This tool has been tested with ghc version 8.2.2.

<h1>Installation instructions</h1>
Run the following commands in terminal:

make

cabal install --enable-tests

The makefile contains instructions for compiling the Agda files. To execute it, you will need Agda 2.5.4. It will end with an error "Could not find module ‘Control.Monad.Combinators.Expr’", but that's okay.

<h1>RefactorAgda</h1>

This project is a refactoring tool for a subset of Agda called Baby-Agda. This subset is intended to be extended gradually to include the entire Agda language. It does not currently have a main function but the functionality can be tested with cabal's testing framework.

<h1>Currently supported refactorings</h1>
Reindent, rename, push (reorder) function arguments, function extraction.

<h1> Missing a refactoring? </h1>
Feel free to open an issue if a desired refactoring is not on this list.
