-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ExplicitTypeApplication #-}

module Main where
import Prelude

data Foo = Foo { first :: Int, second :: Bool} deriving Show

f :: a -> b -> (a,b)
f u v = (u, v)

g :: Int -> Int -> (Int, Int)
g u v = f @(Int) @Int u v

-- Testing different '@' locations
-- weaved :: Float -> Float -> (Float, Float)
-- weaved u v = f @Float u @Float v

-- after :: Bool -> Bool -> (Bool, Bool)
-- after u v = f u v @Bool @Bool

dblTuple :: (a, b) -> ((a, b), b)
dblTuple e@(_,y) = (e, y)


-- interesting note:
-- listpair :: forall a. [a] -> ([a], [a])
-- therefore when explicitly applying, you do NOT put the type in "[ ]"

listpair :: [a] -> ([a], [a])
listpair [] = ([], [])
listpair b@(_:_) = (b, b)

main :: IO ()
main = do
         print $ g 5 12
         print $ ((id @String (concat ["hello", "world", []])):"Hamidhasan":[])
         print $ dblTuple @(Foo) @String ((Foo 5 True), "hello")
         print $ listpair @(Maybe Int) [Just 12, Nothing]
         print $ listpair @(Maybe Bool) $ (Just True) : (Just False) : (Nothing @Bool) : []
         print $ dblTuple @Foo @[Maybe Int] ((Foo 7 False), ([Just 5, Nothing]))
