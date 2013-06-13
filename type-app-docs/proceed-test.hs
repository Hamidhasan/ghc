-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

-- id :: a -> a
-- So the result of applying id should normally give an alpha.
-- However, with explicit type application, this signature,
-- at __application time__ is __redefined__: the 'a' becomes Bool.
 
-- Hence, the function "id &Bool" should actually be a function
-- with type Bool -> Bool. This allows me to use the result - bool -
-- as a parameter in an if-statement (which should be boolean).

-- It seems that Haskell can actually figure this out without
-- needing me to specify it, but I wonder if there is a more
-- complex case in which it cannot.

main :: IO ()
main = let bool = (id &Bool )True in  -- Bool -> Bool
       if (bool) then print "True"
       else print "False"
