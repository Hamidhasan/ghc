-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

{-
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
-}

main :: IO ()
main = do
         print $ show (read &Int "3")
         print $ show &Integer (read "5")
         print $ show &Int (read &Int "7")
