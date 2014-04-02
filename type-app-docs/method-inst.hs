-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ExplicitTypeApplication, RankNTypes, ScopedTypeVariables #-}

module Main where
import Prelude

class C a where
  f :: Eq b => b -> a -> Int
  baz :: Eq a => Int -> a -> Int

instance C Int where
  baz = f

main :: IO ()
main = do
  print "hello"
