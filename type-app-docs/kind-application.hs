-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE KindSignatures, ExplicitTypeApplication, PolyKinds, RankNTypes  #-}

module Main where
import Prelude

data First (a :: * -> *) = F
data Proxy (a :: k) = P -- This expands to P (kind variable) (type variable)
foo :: Proxy a -> Int
foo _ = 0

first :: First a -> Int
first _ = 0

-- fTest = first F
-- fMaybe = first @Maybe F
-- fInt = first @Int F -- should fail

test = foo P
bar = foo @Bool P -- should work

-- baz = foo @Bool (P :: Proxy Int) -- should fail
      
main :: IO ()
main = do
         print $ foo P
