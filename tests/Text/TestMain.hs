module Main where

import Test.HUnit
import Text

main :: IO Counts
main = do 
    runTestTT testTextAtlas
