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

#### Tuesday, June 4

Today I spent most of the time trying to digest and understand how GHC handles Haskell's abstract syntax.
I also read up on GHC coding conventions, style, and other "Before you begin" material.

The most useful information relating specifically to abstract syntax is located here:
http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/HsSynType

I was able to add a small case to HsExpr, but I am not sure of its correctness yet. Furthermore, I may 
need to add other cases and will need to be able to pretty print it (but I need to know its syntax before
I can pretty print). 

#### Wednesday, June 5

I made some more progress on the Abstract Syntax as well as the parser today. I read through almost the entire
Parser.y.pp file, and was not too lost on the changes.

Also, my advisor and I discussed some possible applications and related topics. In particular,
there exists explicit type application for *pattern matching*, but this feature is not really
a part of what I am doing (and has less priority for type application when calling polymorphic functions).

Incidentally, I found an interesting tidbit in the HsExpr syntax. One of the cases is on Line 315, which 
uses explicit type application [in generic programming?]; it may be useful for my project to re-use this
piece of abstract syntax (no parser rule generates this syntax though).


#### Thursday, June 6

I think I have more or less completed the Abstract Syntax piece that needs to be
implemented in the compiler. I added a new node, ETypeApp, that has a type and
another expression that follows so it can be chained (you can multiple explicit
type applications).

However, now I am working on the parser, and have reached a little stumbling 
block. Straightforward, most of the suggested changes in syntax simply do not
work. This will require either adding new brackets, which may be cumbersome,
or some more intricate hacking of the LR grammar. It is possible that my approach
is slightly wrong and I have to modify where the explicit type application can 
fit into the function application syntax (i.e., my current code in the parser is
located in the wrong set of rules).

I may need to create a new token (for example, "{| |}" brackets). However, I am 
not going to do this just yet. If I want to do it, it will require editing the 
Lexer.x (Alex) file, which does not look very straightforward.