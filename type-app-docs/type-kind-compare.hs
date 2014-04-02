-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ExplicitTypeApplication, RankNTypes, PolyKinds, KindSignatures #-}

module Main where
import Prelude
-- fishy
-- Easy
silly :: a -> Bool
silly _ = False

foo = silly @Maybe      -- should fail

-- Harder
data Proxy (a :: k) = Proxy
boo :: forall (a :: * -> *) . Proxy a -> Bool
boo _ = False

base = boo Proxy
bar'= boo @Maybe Proxy -- should work
bar = boo @Int         -- should fail

main :: IO ()
main = do
    --    print $ foo
          print $ bar'
