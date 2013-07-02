-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude


g :: a -> a
g u = u

--f :: Int
f = g &Int 5

main :: IO ()
main = do
         print $ f 
         print $ show (read &Int "3")
