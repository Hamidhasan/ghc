-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ExplicitTypeApplication, RankNTypes #-}

module Main where
import Prelude

{- This test case tests nested foralls, to see if deep skolemisation
   works correctly in conjunction with explicit type application.

   Additionally, mapSame is slightly different, where it uses a
   scoped type variable as an explicit type argument. 
-}

pair :: forall a. a -> forall b. b -> (a, b)
pair x y = (x, y)

pair' :: forall a b. a -> b -> (a, b)
pair' x y = (x, y)

pairnum :: forall a. Num a => forall b. b -> (a, b)
pairnum = pair 3

main :: IO ()
main = do
         print $ pair 3 True
         print $ pair @Int 3 @Bool True
         print $ pair 3 @Int @Bool True
--         print $ pair' 3 @Bool True
         print $ pair 3 @Int @Bool 5 -- correct fails
         print $ (pair 3 :: forall a. Num a => forall b. b -> (a, b)) @Int @Bool True
         print $ pairnum @Int @Bool True
