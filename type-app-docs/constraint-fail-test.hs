-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

addOne :: Num a => a -> a
addOne x = x + 1
 
bool = False

main :: IO ()
main = do
         print $ addOne &Bool True
         print $ addOne &Bool bool
