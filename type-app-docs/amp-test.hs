-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

x :: Int
x = 5

y :: Bool
y = True

f :: a -> b -> (a,b)
f u v = (u, v)

main :: IO ()
main = do 
         print (f &Int &Bool x y)
