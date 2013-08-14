-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

silly :: a -> Bool
silly _ = False

foo = silly @Maybe undefined

main :: IO ()
main = do
         print $ foo
