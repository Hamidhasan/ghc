-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

data Foo = a -> b -> Food a b

type Oof b a = Foo a b

g :: a -> a
g u = u

add :: Int -> Bool -> Int
add x True = x + 1
add x False = x - 1

h :: Int -> Bool -> Int
h x y = add &Int &Bool x y

f :: Int -> Int
f x = g &[Int] [1,2,3]

foo :: (Int -> Bool) -> Int
foo x = f &(Int -> Bool) h &Int

main :: IO ()
main = let x = 5 in
       print $ f x