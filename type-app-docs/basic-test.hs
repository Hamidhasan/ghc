-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

addOne :: Int -> Bool
addOne _ = True

addTwo :: a -> Bool
addTwo _ = True

addFun :: Int -> Bool
addFun x = addOne x
                   
main :: IO ()
main = do
  print 5
  print (addOne 7)
  print (addTwo 2)
  print $ addFun 5
