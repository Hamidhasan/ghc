-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

plus :: Int -> Int -> Int
plus x y = x + y

main :: IO ()
main = do
  print $ plus &Int 5 7
  print $ plus &Rational 5 10
  print $ (+) &Int &Int &Int 12 14
