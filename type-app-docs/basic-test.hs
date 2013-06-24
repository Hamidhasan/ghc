-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude


g :: a -> a
g u = u

--f :: Int
f = g &Int 5

main :: IO ()
main = let x = 5 in
       do
         print $ f
         print $ g &Int 7
