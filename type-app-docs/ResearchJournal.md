# Research Journal for Haskell Explicit Type Application

### Hamidhasan G. Ahmed - Summer 2013 - University of Pennsylvania
### Advised by Dr. Stephanie Weirich
================================================================


## Week 1: June 3 - June 7

#### Monday, June 3

Today my advisor and I talked about the first three major steps of the project.

**1. Add a new node/token/expression to the Abstract Syntax, so that whatever is parsed has a corresponding abstract syntax node to parse to.**

While looking through the files, I believe the type of this node will be a
"HsExpr.HsExpr". All of the abstract syntax files are in the ghc/compiler/hsSyn/
folder, however there are several, and my current task is to figure out which of
these files to modify and how to do so.

A good summary of the compiler in general is located in the CoreSyn.lhs file, 
towards the top. 

**2. Try to figure out the correct syntax to denote type application, then modify parser to accept the new syntax correctly.**

In order for this to be completed, we need to decide on what syntax is best. My
advisor outlined a couple of suggestions, each of which had strengths and 
weaknesses. In particular, some "nicer" syntax styles may have conflicts with
existing syntax ('@' symbol with "as-patterns", '{ }' with record syntax).

I have updated the ExplicitTypeApplication page of the Haskell GHC Wiki with some
of these suggestions, including a link to a feature-request ticket that outlines
exactly this problem and some of the solutions.

Link to wiki page (I have updated a small portion, may continue to do so or start
my own page): http://hackage.haskell.org/trac/ghc/wiki/ExplicitTypeApplication

Link to feature-request ticket: http://hackage.haskell.org/trac/ghc/ticket/4466

**3. Assemble a test suite that demonstrates the semantics of using explicit type application, including corner cases.**

There are various situations that need to be dealt with, and I have not thought 
thoroughly about what cases there are.

For starters, though, my advisor mentioned that the type declarations of a given
polymorphic function (whose arguments would then be given variables that were 
explicitly typed) would be a good place to start. In particular, the ordering of
arguments to "inferred" type declarations is nontrivial: are the type arguments
given left to right, matching the inferred declaration? In addition, getting this
to work with "type synonyms" may be another nontrivial step.

