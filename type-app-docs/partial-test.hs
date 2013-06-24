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
-- Should it simply be omitted, or should we include
-- a &_ indication?
pairup :: a -> b -> (a,b)
pairup x y = (x, y)

-- what will Haskell infer the type of this function to be?
-- my guess: 
-- intpair :: Int -> a -> (Int, a)
intpair x y = pairup &Int x y

-- What about the validity of these:
-- intpair x y = pairup &Int x y
-- intpair x y = pairup &Int x &_ y
-- intpair x y = (pairup &Int x) y
-- intpair x y = (pairup &Int x) &Bool y

-- intpair x y = pairup &Int &_


----------------------------------------------------------------

main :: IO ()
main = do
          print $ intcons 7 []
      
