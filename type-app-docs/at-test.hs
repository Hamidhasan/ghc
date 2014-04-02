-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

data Foo = Foo { first :: Int, second :: Bool} deriving Show

f :: a -> b -> (a,b)
f u v = (u, v)

dblTuple :: (a, b) -> ((a, b), b)
dblTuple e@(x,y) = (e, y)

main :: IO ()
main = do
         print $ f 5 12
         print $ dblTuple ((Foo 5 True), "hello")
