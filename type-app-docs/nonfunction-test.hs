-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

{- While simple, this test case tests if a data constructor
   that takes no parameter - in this case, "None" - can be
   given an explicit type. The type of the print statement
   should be, in this case, "Maybe Int", despite None taking
   no parameter.
-}

main :: IO ()
main = do
         print $ None &Int
