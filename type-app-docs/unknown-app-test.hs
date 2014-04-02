-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

{- This is a very generic, simple use of explicit type application.
   I compare the typechecker's output of functions with and without 
   their signatures, to see if they make a difference internally.
-}

pairup_nosig x y = (x, y)

pairup_sig :: a -> b -> (a,b)
pairup_sig u w = (u, w)

quad :: a -> b -> c -> d -> (a, b, c, d)
quad w x y z = (w, x, y, z)

main :: IO ()
main = do
         print (pairup_nosig (fromInteger 1) False)
         print (pairup_nosig @_ @Bool 5 True)
         print $ pairup_sig @String @_ "Hello!" (fromInteger 5)
         print $ pairup_nosig @_ @_ "Hello!" 'W'
         print $ (+) @Float 5 7
         print $ quad @_ @Int @Bool @_ "Quad!" 5 True 'b'
         print $ quad @Bool @_ @_ @[Char] False "Quad2!" 17 []
