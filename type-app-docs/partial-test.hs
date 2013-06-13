-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

-- a partial application of the cons operator.
-- I think this should typecheck and be valid,
-- but am not sure.

-- intcons should have type: Int -> ([Int] -> Int)
intcons a = (:) &Int a

-- Another thing to consider: explicit partial.
-- Should it simply be ommitted, or should we include
-- a &_ indication?
pairup :: a -> b -> (a,b)
pairup x y = (x, y)

-- what will Haskell infer the type of this function to be?
-- my guess: 
-- intpair :: Int -> a -> (Int, a)
intpair x y = pairup &Int &_ x y

-- What about the validity of these:
-- intpair x y = pairup &Int x y
-- intpair x y = pairup &Int x &_ y
-- intpair x y = (pairup &Int x) y
-- intpair x y = (pairup &Int x) &Bool y

-- intpair x y = pairup &Int &_


----------------------------------------------------------------
-- Another partial application dilemma:
type Foo = a -> Int
--f :: b -> a -> Int
f :: b -> Foo
f x y = 5

-- If one were to explicit type apply f,
-- what order would the type applications come in?


main :: IO ()
main = do
          print $ intcons a []
          print $ (f &Int 5) &Bool True
      
