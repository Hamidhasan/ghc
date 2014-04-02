-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

{-
 This test case only partially applies explicit type application
 allowing polymorphism for the second argument, in particular,
 for the "pairup" and subsequently defined "intpair" function.
   
 This test case tests to see if explicit type application
 can work alongside regular type inference if a function requires
 more than one polymorphic argument types, and the programmer does
 not want to define the type of all of those arguments.
-}

-- intcons should have type: Int -> ([Int] -> Int)
intcons a = (:) &Int a

-- Another thing to consider: explicit partial.
-- Should it simply be omitted, or should we include
-- a &_ indication?
pairup :: a -> b -> (a,b)
pairup x y = (x, y)

-- what will Haskell infer the type of this function to be?
-- my guess: 
-- intpair :: Int -> a -> (Int, a)
intpair x y = pairup &Int x y

-- What about the validity of these:
-- intpair x y = pairup &Int x y
-- intpair x y = pairup &Int x &_ y
-- intpair x y = (pairup &Int x) y
-- intpair x y = (pairup &Int x) &Bool y

-- intpair x y = pairup &Int &_


----------------------------------------------------------------

main :: IO ()
main = do
          print $ pairup &Int 5 True          
          print $ intcons 7 []
      
