-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude


g :: a -> a
g u = u

--add :: Int -> Bool -> Int
add x True = x + 1
add x False = x - 1

-- This seems redundant, but not incorrect.
-- Perhaps it should give a warning?
--h :: Int -> Bool -> Int
h x y = add &Int &Bool x y

main :: IO ()
main = let x = 5 in
       print $ h 5 True
