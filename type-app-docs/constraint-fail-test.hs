-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

{-
This is a test case that shows that even if the function and
argument match, an incorrect type application will not typecheck,
which is an informative outcome to the programmer.

In this case, "addOne 5" would normally work, but since Bool is not
a numeric type, "addOne &Bool 5" does not work and a descriptive
error message is generated.
-}

addOne :: Num a => a -> a
addOne x = x + 1
 
bool = False
 -- TODO: The error message doesn't mention the &Bool
 -- it mentions the "addOne" instead
main :: IO ()
main = do
         print $ addOne @Bool 5
         print $ addOne @Bool True
         print $ addOne @Bool bool
         
