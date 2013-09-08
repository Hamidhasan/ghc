-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, ExplicitTypeApplication, PolyKinds, KindSignatures #-}

module Main where
import Prelude

{- This test case tests nested foralls, to see if deep skolemisation
   works correctly in conjunction with explicit type application.

   Additionally, mapSame is slightly different, where it uses a
   scoped type variable as an explicit type argument. 
-}
pair :: forall a. a-> (forall b. b -> (a, b))
pair x y = (x, y)

triple :: forall a. a -> forall b. b -> forall c. c -> (a, b, c)
triple x y z = (x, y, z)

sid :: (forall a. a -> a) -> (forall b. b -> b)
sid x = x
bar = (sid @(Int) id) 5

many :: forall a (b :: * -> *). Ord a => a -> b a -> forall c. [c] -> forall d . Num d => d -> (a, b a, [c], d)
many a b c d = (a, b, c, d)

main :: IO ()
main = do
         print $ pair 5 False
         print $ pair @Int 3 @Bool True
         print $ triple @Int 12 @Float 5 @String "Hello"
         
         print $ many @Int @Maybe 5 Nothing @Char "Hello" @Float 17
