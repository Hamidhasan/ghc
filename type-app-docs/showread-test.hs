-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

main :: IO ()
main = do
         print $ show 3
         print $ show &Int 5
 --        print $ show &Int (read &Int "7")
