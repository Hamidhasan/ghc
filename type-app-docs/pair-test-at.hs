-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ExplicitTypeApplication #-}

module Main where
import Prelude

{- This is a very generic, simple use of explicit type application.
   I compare the typechecker's output of functions with and without 
   their signatures, to see if they make a difference internally.
-}

pairup_nosig x y = (x, y)

pairup_sig :: a -> b -> (a,b)
pairup_sig u w = (u, w)

main :: IO ()
main = do
         print $ pairup_sig @Bool @Int False 7
         print (pairup_sig @Bool @Int False 7)
         print (pairup_nosig (fromInteger 1) False)
         print (pairup_nosig @Int @Bool (fromInteger 5) True)
         print $ pairup_sig @Bool @Int False 7
