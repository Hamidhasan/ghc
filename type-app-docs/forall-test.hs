-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ExplicitTypeApplication, RankNTypes, ScopedTypeVariables #-}

module Main where
import Prelude

{- This test case tests nested foralls, to see if deep skolemisation
   works correctly in conjunction with explicit type application.

   Additionally, mapSame is slightly different, where it uses a
   scoped type variable as an explicit type argument. 
-}

pair :: forall a. a-> (forall b. b -> (a, b))
pair x y = (x, y)

sid :: (forall a. a -> a) -> (forall b. b -> b)
sid x = x

foo :: forall a b. a -> b -> (a, b)
foo x y = (x, y)

bar :: forall a. a -> forall b. b -> (a, b))
bar x y = (x, y)

main :: IO ()
main = do
         print $ pair 3 True
         print $ pair 3 True
         print $ foo 3 5
         print $ bar 3 5
  --       print $ sid &(Int -> Int) (+ 5)
