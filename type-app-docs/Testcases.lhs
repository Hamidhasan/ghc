%
% Explicit Type Application Testcase and Information
% Hamidhasan G. Ahmed, University of Pennsylvania, ahmedh@seas.upenn.edu
% Advised by Dr. Stephanie Weirich, University of Pennsylvania, sweirich@cis.upenn.edu
% 

\section{Prologue}
The premise behind this document is to explain the design decisions
behind implementing Explicit Type Application, and 
use cases that are interesting and characterize the feature.

Some of the design decisions made in the following sections are also found
as adapted comments within GHC's code itself, formatted according to the
"Note" style guidelines that GHC uses. The explanations here have additional
information than the comments - this information is denoted by the | | around
such paragraphs.

\section{Parsing}
WIP. Currently pending a second pass where I may reappropriate '@'.
Right now, we currently use '&'.

If '&' is to be used in the final iteration, the following explains the decision:

In core, '@' is the symbol used for explicit type application. However,
it is difficult to use '@' for Haskell syntax, since '@' is used for patterns
which binds tighter than expressions, and results in conflicts where the parser,
instead of detecting a type application, detects a pattern, and thus fails with
"Pattern found in Expression Context". We also surmised about using {Type}, but
because of the rich record syntax, this was also impossible. We thus finally
decided to use '&', which currently is an "free" character (not reserved) and
can be defined as an infix function by the programmer.

Currently, enabling this extension will make '&' a reserved character and
make it unable to be used as an infix function. However, '&&' will still work
(by default, defined as "logical AND" in Prelude). The downside to this is that
if one to import a library that defines, or defines themselves, an infix function
using '&', they will not be able to use this extension, and vice versa (the two
will conflict; '&' will take precedence).

\section{Typechecker}
tcApp - There are a couple of design decisions regarding tcApp that are important.

Note [Threading Explicit Type Application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In order for explicit types to be available to the function's application, but
_not_ the [first-class function] arguments to the function being applied, 
the explicit types have to be threaded through seperately until all collected,
then they can be placed within the local environment.

This is done through recursion, similar to the way the function's arguments
are currently handled.

| Unfortunately, this is a bit destructive to the current tcApp, since an
additional argument of type [LHsExpr Name] is required. However, it is clearer to read,
and callers of the function did not need to change in several places- where it did
need a change, a simple [] (empty list) will suffice for the new argument.|

When the call is made to tcInferFun, the types are then injected using
"setLclTypeApps", which then puts them within the local environment. Then,
from instantiateOuter, they can be pulled out of the local environment
and used to typecheck the function and instantiate its type variables to
those explicit types.

| The reason we use the local environment from tcInferFun onwards is twofold.
First, we wanted to minimize the number of function type signatures to change
(tcInferFun, tcInfer, instantiateOuter, would all have to change if we did not),
which is very messy. Second, by putting explicit types into the local environment,
it can be easily accessed and overwritten, and is a better foundation if in the
future, another contributor wishes to expand upon the feature. |


Note [Set Local Type Applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This function sets the explicit type application in the local
environment and checks each type in the process, to see if the type
is well-formed itself.

However, since it modifies the environment, one could argue it should be in
TcRnMonad.lhs, but there, "tcLHsType" is not defined (since TcType.lhs
is not imported in TcRnMonad.lhs).

| Ultimately, it is a bit of a cross-cutting function that can fit in
more than one place - I feel it belongs here more, as it is closely
related to the tcApp function and would feel detached in TcRnMonad.lhs. |


Note [Explicit Type Application] ("instantiateOuter" function in TcExpr.lhs)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| This is a detailed explanation of how Explicit Type Application is performed
in the typechecker/TcExpr.lhs:instantiateOuter function. It is where most of the
logic behind typechecking and compiling the feature is located. |

First, we get the explicit types from the local environment.

We then check to see if the programmer has provided more explicit types than
type variables available. In this case, this is a user error, and a descriptive
error message is generated.

After this, we split the list of type variables into two sublists; the first
list with the type variables we will explicitly substitute into (and we use
the length of our explicit type list to determine this number).

In order to preserve the invariant that a TvSubst should only be applied once,
we combine the two TvSubsts. In theory, the two type variable substitutions
should be disjoint, otherwise, there could be unintended behavior.

Finally, we compose two wrappers: wrap, generated normally by instCall, and
(mkWpTyApps etypes), made out of our explicit types. These two combine into
a wrapper that wraps the resulting function with all the type information it
needs. This allows for the explicit type information to propagate down to the
Desugarer and Core correctly without any issues.

| Which type variables get instantiated, and in what order does it happen?
It currently follows the left-to-right order of the appearance in the type
signature. See the example:

   f :: forall a. b. a -> b -> (a, b)
   f &Int &Bool x y

   Also works:
   f &Int x &Bool y

   In both cases: Int -> a; Bool -> y

I feel that this makes the most intuitive sense for the programmer. |

| Here, there are essentially two approaches to the problem: (1) split off the
tvs from the entire list, substitute in the explicit types seperately, and then
combine with the rest of the tvs that were instantiated with metatype variables,
or (2) throw explicit types into the constraint solver and attempt to solve it.
I chose path 1, because it is clearer, and it seems the constraint solver does
not expect a forall type to be able to instantiate immediately - I was unable
to generate evidence correctly using the constraint solver when dealing with
type classes. However, it is used for checking instances, and errors such as
providing an Explicit Type of "Bool" when a "Num a" is needed will fail correctly. |



\section {Interesting Testcases}

The testcases follow a specific format: First, there are some functions defined,
usually polymorphic ones, then a "answer" field which uses the explicit
type application. Then, the comment next to the answer shows the value of the
variable. Each test is preceded by a comment that explains the interesting nature.

pair-test.hs
~~~~~~~~~~~~
This is a very generic, simple use of explicit type application.
Type application can be performed with or without type signatures on polymorphic
functions.

Note that, while this is useful for the programmer, type application provides
no extra functionality in this testcase; the following testcase would work
just as well without any type application.

\begin{code}

pairup_nosig x y = (x, y)

pairup_sig :: a -> b -> (a,b)
pairup_sig u w = (u, w)

answer_nosig = pairup_nosig &Int &Bool 5 True
-- (5, True) :: (Int, Bool)

answer_sig = pairup_sig &Bool &Int False 7 --
-- (False, 7) :: (Bool, Int)

\end{code}

showread-test.hs
~~~~~~~~~~~~~~~~
This test case shows how using explicit type application resolves ambiguity, by
using the infamous "show/read" problem as an example. (The show/read problem is
a problem in which the answer to using show followed by read - 'show (read "3")'
should be obvious, but fails in the typechecker because of an ambiguity in the
constraints).

Using type application on either of the functions, or both, allows the compiler
to correctly typecheck the program. In this scenario, type application does infact
provide power to the programmer - unlike in "pair-test.hs", it is
necessary in order for the answers to typecheck.

\begin{code}

answer_read = show (read &Int "3") -- "3" :: String
answer_show = show &Integer (read "5") -- "5" :: String
answer_showread = show &Int (read &Int "7") -- "7" :: String

\end{code}

constraint-fail-test.hs
~~~~~~~~~~~~~~~~~~~~~~~
This is a test case that shows that even if the function and
argument match, an incorrect type application will not typecheck,
which is an informative outcome to the programmer.

In this case, "addOne 5" would normally work, but since Bool is not
a numeric type, "addOne &Bool 5" does not work and a descriptive
error message is generated.

The error message reads:
    No instance for (Num Bool) arising from a use of ‛addOne’
    In the expression: addOne (&Bool) 5
    In an equation for ‛answer_fail’: answer_fail = addOne (&Bool) 5

\begin{code}

addOne :: Num a => a -> a
addOne x = x + 1
 
 -- Note: The error message doesn't mention the "&Bool"
 -- it mentions the "addOne" instead. I think it currently
 -- is sufficiently informative, however.
           
answer_constraint_fail = addOne &Bool 5
-- The result is illtyped. The error message is above.
-- Comment this out, for the rest of the code to compile.

main = return ()

\end{code}

partial-test.hs
~~~~~~~~~~~~~~~
This test case only partially applies explicit type application
allowing polymorphism for the second argument, in particular,
for the "pairup" function.
   
This test case tests to see if explicit type application
can work alongside regular type inference if a function requires
more than one polymorphic argument types, and the programmer does
not want to define the type of all of those arguments.

It also uses "intcons", to see if a built-in operator like (:) can
be explicitly typed.

\begin{code}

intcons a = (:) &Int a

pairup :: a -> b -> (a,b)
pairup x y = (x, y)

intpair x y = pairup &Int x y

answer_pairup = pairup &Int 5 True -- (5, True) :: (Int, Bool)
answer_intpair = intpair 1 "hello" -- (1, "hello") :: (Int, String)
answer_intcons = intcons 7 []      -- [7] :: [Int]

\end{code}

hard-test.hs
~~~~~~~~~~~~
This testcase deals with type families. Normally,
the declaration "g True" would be ill-typed, even though
the needed type is "&Char", but because type families
are a relatively new feature, this is ill-typed.

However, the result of answer is still Prelude.undefined.
TODO: This may require some more investigation.

\begin{code}

-- To be used with the following, otherwise the code will
-- not compile.
-- {-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

type family F a
type instance F Char = Bool

g :: F a -> a
g _ = undefined

f :: Char
f = g &Char True

answer = g &Char False

\end{code}

________________________
TODO List for Testcases:
~~~~~~~~~~~~~~~~~~~~~~~~
Currently, three testcases don't work, glaringly. I am not making them
appear as code for now, otherwise they will mess up the file.

lambda-test.hs
~~~~~~~~~~~~~~
   (\x -> x) &Int 12
This fails to compile. It is being weird, applying the
explicit type too deep within the lambda, where it does not
have a forall type yet. (Or, it isn't giving a forall type to
the lambda. I tried to give the lambda an explicit forall signature
however, and the same error happened).

answer_lambda = (\x -> x) &Int 12

nested-forall-test.hs
~~~~~~~~~~~~~~~~~~~~~
This isn't working because, as I suspected, it fails when there is
a nested forall type in the type signature (It doesn't "see" the
type variables).

------
mapSame :: forall b. (forall a. a -> a) -> [b] -> [b]
mapSame _ [] = []
mapSame fun (x:xs) = fun &b x : (mapSame &b fun xs)

pair :: forall a. a-> (forall b. b -> (a, b))
pair x y = (x, y)

sid :: (forall a. a -> a) -> (forall b. b -> b)
sid x = x

main :: IO ()
main = do
         print $ pair &Int &Bool 3 True
         print $ sid &(Int -> Int) (+ 1)
         print $ mapSame (id) [1, 2, 3]
------

nonannotated-recursive.hs
~~~~~~~~~~~~~~~~~~~~~~~~~
Here, I don't think that the explicit type
application works well with scoped type variables. It is giving me
a constraint error which shouldn't really be there, if I use (+),
for example (though it should not happen).

mapSame :: forall b. (forall a. a -> a) -> [b] -> [b]
mapSame _ [] = []
mapSame fun (x:xs) = (fun &b x) : (mapSame &b fun xs)

main :: IO ()
main = do
         print $ mapSame &Int (+1) [1, 2, 3]

