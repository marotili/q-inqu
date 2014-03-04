module Main where

import Test.HUnit
import Wire

main :: IO Counts
main = do 
	runTestTT testLoadMap
