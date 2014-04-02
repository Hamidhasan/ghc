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

## Week 2: June 10 - June 14

#### Monday, June 10
Today the parsing portion of the task was, at least initially, completed. This required a slight change to the
intended abstract syntax, and after changing it, I was able to put in a new rule using the '&' character, that
added no new shift/reduce conflicts to the parser. In order to add the rule, I had to also modify the lexer to
recognize the ampersand token.

The renamer portion was also completed - this time, by a previously unknown contributer who attempted to add the
Explicit Type Application in the past, but stopped because they could not get it through the type checker (and left
a comment saying that they could not). They had correctly implemented the renamer portion though (although it may
require some sanity checking - it does, however, go through without error, and the test files now give "type checker"
error messages).

My advisor recommended that at this point, I should write up test cases, and run my current compiler through the test
suite to see if it passes all of the current GHC test cases. Currently, the '&' is always enabled, and not an
extension; however, it will most likely become an extension once the project is completed (That also means that
when using it with code, the '&' will become a reserved character, although the syntax is still subject to change).
I should also look at the GHC test suite and assemble some of my own test cases, to see what should and should not
fail.

#### Wednesday, June 12

Between today and yesterday, the build configuration on my virtual machine got corrupted, and it was 
randomly compiling certain modules incorrectly. Redownloading and remaking from scratch did not seem
to work, so I had to do the unthinkable: build it on Windows. This took a large portion of time but
I was able to actually get almost the entire working environment set up on Windows - and it's not
that bad! The added speed and ability to use all four cores while running make or the testsuite also
helps.

Before rebuilding on Windows, however, I ran the entire testsuite on the virtual machine - on both
branch master and type-app. Both reported the exact same test results, so it seems that my initial
changes to the parser and lexer are not breaking anything in the build process (though the feature
remains incomplete in the typechecker). As of right now, however, I believe that the addition 
restricts the use of the '&' character (usually it is up for grabs as an infix function definition).

I have begun to write some very small test cases, as well as reading up on how the typechecker works
in order to figure out how to implement explicit type application. There are a lot of subtleties to
consider, so this part will most likely be the most difficult part of the project.

#### Thursday, June 13

I spent the first half of today writing up more detailed testcases, and some had proposed edge cases that
tested the limits of what Explicit Type Application should and should not be able to do. I now have a
larger test suite that demonstrates how using the feature would look like.

I also met with my advisor to provide a status report of what I have done so far, show her the test cases,
and talk about the next challenge: semantics, and/or getting explicit type application through GHC's
typechecker. My advisor gave me a quick and effective introduction to how polymorphic typechecking works,
and pointed me to two papers that I should look over to help me understand the type system more completely.

The two papers are located here: http://repository.upenn.edu/cis_papers/315/ 
and http://research.microsoft.com/apps/pubs/default.aspx?id=162516

## Week 3: June 17 - June 21

#### Tuesday, June 18
Yesterday and today, I thoroughly read through the first of the two research papers, and began 
reading the second. The first paper was very informative, and described theoretically how a 
Haskell-like type system should work; then, proceeded to give an actual Haskell implementation.

This implementation mirrored what is in the compiler/typecheck/ directory in GHC; many of the
variables are named similarly, and it is helping me to understand the code in GHC's typechecker.

I also figured out how the typechecker is able to accumulate all the arguments of a function, 
and then how it typechecks it. I am still not sure how GHC exactly resolves the type arguments;
but I know where in the code it attempts to do so. 

The next stage, after reviewing the second paper (which is more constraint-based but also a bit
more in-depth than the first), will be to **create some sample judgements/typing rules that**
**demonstrate the theoretical use of the new rule: Explicit Type Application**. For this part of
the assignment, it will be necessary to solve the problem theoretically before diving into the code
and attempting to blindly solve it in the gigantic GHC compiler.

#### Thursday, June 20th
I have finished reading both papers [to the best of my limited ability!]. I now have a much better
grasp of what is going on behind the type checker. 

I have not made judgments yet, but I have been poking around within the compiler itself to see 
how function applicatons are compiled. At this point, I have narrowed down the functions that I need
to change to about two or three specific ones. 

It is possible that in order to implement this feature, I will have to treat ETypeApp - basically,
the flagger for the variable - as a "seperate" application and re-do all the typing rules for it -
basically, treat the entire function differently if it sees an type application. This is what I 
tried to do initially in the parser, but it did not seem to work well. 

This may need to be done because the type application(s) need to create new meta TyVar[iable]s and 
then re-unify with the already-checked function type. Basically, the function type, after it is 
checked, needs to be checked against the type application(s) to see if the function's argument-types
are more polymorphic than what is supplied by the type application. To do this, it may require a 
greater context - one that can be provided if we implement a seperate "app" rule.

However, we could just also pass information back - this is the way it will be done if I stick with
the current approach. This will ruin the nicely-built map function in the typechecker, which I 
hesitate to do, but may need to end up doing.

After creating the judgments, I will try to investigate this further. For now, I will stick with 
the current approach - ETypeApp is a abs. syntax element that only appears as a type, and as an arg
of a function - no special "app" rule is needed to typecheck it.

## Week 4: June 24 - June 28

#### Monday, June 24
Today, I was able to print out "warning" messages that [in an application using
Explicit Type Application] inform me what the types of the function, expected 
arguments, and explicit types, all are. In the case of polymorphic types, an 'a'
followed by an underscore and random letters are printed out. 

This gives me a sense of what types the typechecker currently has and is
expecting. However, I'm still not sure how to check that this is a forall type,
nor do I currently know how many variables are bound by the forall statement.

A couple of questions I will need to face down the road:
 - If a function has no forall type, and the programmer puts a type application,
   should this be an error? I would think so - but then there must be a check
   to see that the function is a forall type. [This is part of the current task].
 
 - If the function has forall a b. (...), but the programmer only puts one
   explicit type application, what should occur? Should the first variable be
   explicitly typed, but the second is still free to bind with something else?
   
 - In the above context, what constitutes "the second variable", since the
   forall statement does not have an ordering of variables (and the ordering 
   can be nontrivially changed with the use of type synonyms)?

#### Friday, June 28th
After meeting with my advisor yesterday, we were able to figure out where exactly to 
inject the explicit types into the typechecker, as I was having trouble trying to
find where the type variables were instantiated with meta-variables. 

We also talked about threading the explicit types through the "local environment" of
the typechecker monad. I completed most of the local-environment integration today,
and now should be able to start working on actually instantiating the type variables
with explicit types, through the tcInferId -> instantiateOuter -> ... chain of functions.

## Week 5: July 1 - July 5

#### Tuesday, July 2
Success! I was able to get the first preliminary explicit types swapped into the function 
signature. This was achieved after a large modification to the instantiateOuter function, 
and it fails in most cases, but it does work for the most basic type substitutions. The 
current implementation cannot substitute over type constructors, though this may not be too
hard to solve.

The bigger issue is that the "tcInfer" function is called implicitly and a lot more often 
then I thought, thus failing randomly. For example, the show/read problem should be fixed 
with just my implementation, but I think it is checked somewhere else before it can hit the 
explicit type substitution and does not compile.

Additionally, I was able to figure out the how the typechecker monad works, and made some
modifications to keep the explicit types around in part of its local environment. This
has made the code look much cleaner and conceptually nicer - it is clear, now, when we 
decide to keep the types in the typechecker's "state". 

After finding where to inject the explicit types, my advisor suggested two strategies to 
perform the instantiation: either to split up and instantiate before trying to solve 
constraints, or use the explicit types as additional constraints and let the constraint-
solver figure out everything. I chose the first route; it seems that with polish, it will
probably work in all cases, but the second route may be "nicer" in terms of code style
and relationship with theory.

#### Monday, July 8th
I have been able to fix many of the cases from the previous week. However, many harder cases
remain.

I am leaving this link here that explains "foralls" quite nicely: 
http://stackoverflow.com/questions/3071136/what-does-the-forall-keyword-in-haskell-ghc-do

I also met with my advisor today, and we discovered some new problems with the typechecker and
how to fix them. Additionally, I need to start organizing and documenting my test cases to
show why certain test cases are important, what use case/interesting property is being
asserted in each, etc.

#### Thursday, July 11th
Over the past few days, I was able to fix most of the problems occuring within the typechecker
portion of GHC, and my code passes through the typechecker...

...only to hit a panic within the DeSugarer/Core transformation. For some reason, the
function "coreSyn/MkCore:mkCoreApp" is getting a forall type, when it should only be
receiving a function type (i.e., no type variables). It only happens in programs that
contain explicit type application, it seems. 

I could easily fix it by adding the case, but it seems that I am breaking an invariant or
transformation somewhere - the function should not be given a foralltype, and yet it is
somehow getting one (despite the explicit type application transformation!) 

Debugging this section is fairly difficult, as I am not sure where the error lies - there
is a problem with the handover from TC to DS, but it could be in either level, or both.

#### Monday, July 29th
After more than a week of build fixing, numerous attempts at trying to fix the desugarer
bugs, introducing more, and then squashing them and refactoring as we went along, I have
a more complete version of the feature in place. Now, several more testcases pass, 
including ones with constraints, and one of the "hard" ones.

It seems that the scoped-type variable test cases are not passing and sometimes can cause
the compiler to go into an infinite loop! Additionally, there are some problems with the 
parser, and ampersand.

Thus, the next few steps are to:
1. Run the testsuite on the new compiler, with old code -> sanity check
2. make this an extension in the lexer, instead of always-enabled (can do this later)
3. Refactor parser code, make it better, not appear in parenthesis, not invalidate &&
4. Check "scoped type variables" test cases ("nested-forall", "nonannotated-recursive")
   and try to solve the bugs with them.
5. Clean up documentation and comments within compiler code.
