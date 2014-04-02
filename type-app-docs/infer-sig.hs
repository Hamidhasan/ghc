
-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ExplicitTypeApplication, ScopedTypeVariables, RankNTypes #-}

module Main where
import Prelude

data Foo = Foo { first :: Int, second :: Bool} deriving Show
-- suggested two cases by R. Eisenberg
newtype N = MkN { unMkN :: forall a. Show a => a -> String }

n = MkN show

-- boo :: Bool -> String --(compiler doesn't infer this type! It infers a -> String!)
boo = unMkN (n @Bool)

main :: IO ()
main = do
         print $ 5
