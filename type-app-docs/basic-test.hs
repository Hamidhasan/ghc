-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude


g :: a -> a
g u = u

f :: a -> a
f x = g &[Int] [1,2,3]

main :: IO ()
main = let x = 5 in
       do
         print $ f x
         print $ g &Int 7
