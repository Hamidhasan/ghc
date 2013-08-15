-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE KindSignatures, ExplicitTypeApplication, PolyKinds #-}

module Main where
import Prelude

data Proxy (a :: k) = P
foo :: Proxy a -> Int
foo _ = 0

bar = foo @Bool P

--baz = foo @Bool (P :: Proxy Int)
      
main :: IO ()
main = do
         print $ foo
