-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

f :: a -> b -> (a,b)
f u v = (u, v)

g :: Int -> Int -> Int
g u v = f @Int @Int u v

--data Foo = Foo { first :: Int, second :: Int} deriving Show

main :: IO ()
main = let x = 5 in
       print $ g @Int x
