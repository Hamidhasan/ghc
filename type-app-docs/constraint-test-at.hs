-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

addOne :: Num a => a -> a
addOne x = x + 1

int = 5

main :: IO ()
main = do
         print $ addOne @Int 7
         print $ addOne @Int int
