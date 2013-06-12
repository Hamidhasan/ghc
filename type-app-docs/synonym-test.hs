-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

data Foo a b = Foo a b deriving Show
type Oof b a = Foo a b

-- Type of Foo constructor: forall a b . a -> b -> Foo a b
-- Type of Oof constructor: forall b a . b -> a -> Foo a b

g :: a -> a
g u = u

-- No type sig
pairup x y = (x, y)

h (Foo a b) = pairup a b
h _ = undefined

f (Foo a b) = pairup &Int &Bool a b
f _ = undefined

-- What will occur if f is called with foo and oof?

main :: IO ()
main = let foo = Foo 5 True :: Foo Int Bool  in
       let oof = Foo 8 False :: Oof Bool Int in
       do
         print "foo"
         print $ (f oof) == (f foo)
         return ()
