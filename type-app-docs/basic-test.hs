-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

addOne :: Int -> Int
addOne x = x + 1

main :: IO ()
main = do
         print (addOne 2)
