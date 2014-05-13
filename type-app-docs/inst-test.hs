-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE RankNTypes #-}

module Main where
import Prelude

{- This test case tests nested foralls, to see if deep skolemisation
   works correctly in conjunction with explicit type application.

   Additionally, mapSame is slightly different, where it uses a
   scoped type variable as an explicit type argument. 
-}

--pair :: forall a. a -> forall b. b -> (a, b)
--pair x y = (x, y)

pair' :: forall a b. a -> b -> (a, b)
pair' x y = (x, y)

plus :: Int -> Int -> Int
plus x y = x + y
--f :: (forall a b. a -> b -> (a,b)) -> Int
--f _ = 5

--pairNoSig x y = (x, y)

main :: IO ()
main = do
         print $ plus 5 12
         print $ pair' 3 True
{-         print $ f pair
         print $ f (pairNoSig)
         print $ pair' 4 False
         print $ (fst (pair, pair')) 5 False
         print $ (head [pair, pair]) 5 False
         print $ (\x y -> (x, y)) 7 True
  -}       
--print $ pair @Int 3 @Bool True
 --      print $ pair 3 @Bool True
--         print $ pair' 3 @Bool True
--         print $ pair 3 @Int @Bool True
         
