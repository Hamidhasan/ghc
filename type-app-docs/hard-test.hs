-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module Main where
import Prelude

type family F a
type instance F Char = Bool

g :: F a -> a
g x = undefined

f :: Char
f = g &Char True

addOne :: Int -> Int
addOne x = x + 1

main :: IO ()
main = do
         print $ f
         print $ g False
         print $ addOne &Int 4
