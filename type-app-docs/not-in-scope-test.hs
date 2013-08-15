-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed
 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ExplicitTypeApplication #-}

module Main where
import Prelude

main :: IO ()
main = do
         print $ id @Foo undefined
