
-- Test File for Explicit Type Application
-- Hamidhasan G. Ahmed

 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ExplicitTypeApplication, ScopedTypeVariables, RankNTypes #-}

module Main where
import Prelude

newtype N = MkN { unMkN :: forall a. Show a => a -> String }

n = MkN show

-- boo :: Bool -> String --(compiler doesn't infer this type! It infers a -> String!)
boo = unMkN @Bool n

-- foo :: Bool -> String
-- foo = unMkN n @Bool -- fails?

main :: IO ()
main = do
         print $ 5
