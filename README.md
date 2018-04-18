This tool has been tested with ghc version 8.2.2.

This project is a refactoring tool for a subset of Agda called Baby-Agda. For details on the refactorings, run the program with the --help flag. Baby-Agda has the following syntax:

Identifiers
Baby-Agda identifiers mostly correspond to Agda name parts. The only reserved words are "import", "-> ", "where", "=", "?", ":" and integer literals, which are handled separately. Identifiers must not start with "-{}-".

Comments
Baby-Agda's comment syntax is mostly identical to that of Agda, and comments may appear anywhere they could in Agda without causing errors. However, in most positions they are currently thrown away during refactoring. They are always preserved between top-level constructs but only in particular places inside them, as noted below.

Top-level constructs
The following make up Baby-Agda code. Any instance of any of these must start on a new line and not be indented. Unless otherwise stated, the instance may be distributed onto multiple lines by indenting all lines but the first.

Holes may appear anywhere where they can in Agda. If they have content, that content is preserved during refactoring.

Pragmas
Baby-Agda includes the BUILTIN pragma in order to make example files easier to write. The pragma syntax ought to be identical in all respects to that of Agda; any difference is a bug. A noteworthy point is that pragmas, unlike other constructs, can be distributed on different lines without indenting and that comments may not appear anywhere in them.

Function types
Function types work in the same way as in Agda, with both explicit and implicit arguments. There must be an arrow "->" after each argument type. There are no underscores or other advanced features or syntactic sugar. Comments are preserved if they appear immediately before or after the colon.

Function definitions
These also are like Agda in its simplest shape. Pattern matching is allowed, but underscores are not. The right-hand side consists either of an identifier or a plain function application.

Data types
These behave in the same fashion as in Agda, and may have both parameters and indices. Parameters must have their type explicitly specified. In the constructors, comments are preserved around the colon.
