-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

x = True

y = 2 + 5

f :: a -> b -> (a,b)
f x y = (x, y)

g :: a -> a
g x = x

main :: IO ()
main = g {|Int|} y
