-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ExplicitTypeApplication, ScopedTypeVariables, RankNTypes #-}

module Main where
import Prelude

data Foo = Foo { first :: Int, second :: Bool} deriving Show

f :: a -> b -> (a,b)
f u v = (u, v)

g :: Int -> Int -> (Int, Int)
g u v = f @(Int) @Int u v



dblTuple :: (a, b) -> ((a, b), b)
dblTuple e@(_,y) = (e, y)


-- interesting note:
-- listpair :: forall a. [a] -> ([a], [a])
-- therefore when explicitly applying, you do NOT put the type in "[ ]"

listpair :: [a] -> ([a], [a])
listpair [] = ([], [])
listpair b@(_:_) = (b, b)

-- suggested two cases by R. Eisenberg
newtype N = MkN { unMkN :: forall a. Show a => a -> String }
n = MkN show
foo :: Bool -> String
foo = unMkN n @Bool   -- Fails without parens! Not anymore!
foo = unMkN (n @Bool)

(&&) :: Bool -> Bool -> Bool
(b@True) && True = True
_ && _ = False

main :: IO ()
main = do
         print $ g 5 12
         print $ ((id @String (concat ["hello ", "world ", []])):"Hamidhasan":[])
         print $ dblTuple @(Foo) @String ((Foo 5 True), "hello")
         print $ listpair @(Maybe Int) [Just 12, Nothing]
         print $ listpair @(Maybe Bool) $ (Just True) : (Just False) : (Nothing @Bool) : []
         print $ dblTuple @Foo @[Maybe Int] ((Foo 7 False), ([Just 5, Nothing]))
