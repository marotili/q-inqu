module Main where

import Test.HUnit
import World

main :: IO Counts
main = do runTestTT testAddRemove
