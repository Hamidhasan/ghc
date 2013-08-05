-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

{- This is a very generic, simple use of explicit type application.
   I compare the typechecker's output of functions with and without 
   their signatures, to see if they make a difference internally.
-}

main :: IO ()
main = do
         print $ (\x -> x) &Int 12
