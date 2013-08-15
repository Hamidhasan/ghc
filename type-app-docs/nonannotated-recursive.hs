-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
--
module Main where
import Prelude

{-
   This test case tests to see if recursion can work
   in the case of nonannotation for a scoped type variable
   case. Currently, this is not possible in GHC, but
   explicit type application may be able to let the function
  (in this case, mapSame) be recursive.
-}

-- mapSame :: forall a b. (a -> b) -> [a] -> [b]
mapSame :: forall b . (forall a. a -> a) -> [b] -> [b]
mapSame _ [] = []
mapSame fun (x:xs) = (fun @b x) : (mapSame fun xs)

plusOne :: Num a => a -> a
plusOne x = x + 1

main :: IO ()
main = do
         print $ mapSame (id) [1, 2, 3]
         print $ mapSame @Float (id) [1, 2, 3]
--         print $ mapSame @Int (plusOne @Int) [1, 2, 3]
