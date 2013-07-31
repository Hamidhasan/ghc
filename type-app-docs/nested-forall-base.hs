-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Main where
import Prelude

{- This test case tests nested foralls, to see if deep skolemisation
   works correctly in conjunction with explicit type application.

   Additionally, mapSame is slightly different, where it uses a
   scoped type variable as an explicit type argument. 
-}

mapSame :: forall b. (forall a. a -> a) -> [b] -> [b]
mapSame _ [] = []
mapSame fun (x:xs) = fun &b x : (mapSame &b fun xs)

pair :: forall a. a-> (forall b. b -> (a, b))
pair x y = (x, y)

sid :: (forall a. a -> a) -> (forall b. b -> b)
sid x = x

main :: IO ()
main = do
         print $ pair &Int &Bool 3 True
         print $ pair &Int 3 &Bool True
         print $ mapSame (id) [1, 2, 3]
