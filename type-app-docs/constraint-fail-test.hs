-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

addOne :: Num a => a -> a
addOne x = x + 1
 
bool = False
 -- TODO: The error message doesn't mention the &Bool
 -- it mentions the "addOne" instead
main :: IO ()
main = do
         print $ addOne &Bool 5
         print $ addOne &Bool True
         print $ addOne &Bool bool
         
