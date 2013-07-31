-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude hiding ((&&))

x :: Int
x = 5

y :: Bool
y = True

f :: a -> b -> (a,b)
f u v = (u, v)

double :: a -> (a,a)
double u = (u,u)

g :: (a, b) -> (b, a)
g (u, v) = (v, u)

h = g.double

(&&) :: a -> b -> a
u && _ = u

main :: IO ()
main = do 
         print (x && y)
         print (f &Int &  Bool x y)
         print (h x)
