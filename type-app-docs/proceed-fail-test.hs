-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

-- id :: a -> a
-- It seems that Haskell can actually figure this out without
-- needing me to specify it, but I wonder if there is a more
-- complex case in which it cannot.

main :: IO ()
main = if (id True) then print "True"
       else print "False"

