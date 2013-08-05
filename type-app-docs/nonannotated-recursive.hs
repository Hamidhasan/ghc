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

mapSame :: forall b. (forall a. a -> a) -> [b] -> [b]
mapSame _ [] = []
mapSame fun (x:xs) = (fun &b x) : (mapSame &b fun xs)

main :: IO ()
main = do
         print $ mapSame &Int (+1) [1, 2, 3]
