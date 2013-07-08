-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude


pairup_nosig x y = (x, y)

pairup_sig :: a -> b -> (a,b)
pairup_sig u w = (u, w)

repeated_pair u = (u, u)

partial_pair x = pairup_sig &Int

main :: IO ()
main = do
         print (pairup_nosig &Int &Bool (fromInteger 5) True)
         print $ pairup_sig &Bool &Int False 7
         print $ repeated_pair &Int 12
