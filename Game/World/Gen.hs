{-# LANGUAGE NamedFieldPuns #-}
module Game.World.Gen 
    (
    ) where

import System.Random
import Data.Maybe
import Debug.Trace
import Test.QuickCheck

data FreqDist a b = FreqDist
    { freqDist :: [(a, b)]
    }

getDist :: (Show b, Num b, Ord b, Random b, RandomGen g) => g -> FreqDist a b -> (a, g)
getDist g FreqDist { freqDist } = (result, g')
    where
        range = (0, (sum . map snd $ freqDist) - 1)
        (randVal, g') = randomR range g
        result = fromJust . fst $ foldr findResult (Nothing, 0) freqDist

        findResult (a, b) (Nothing, s) = if s + b > randVal then (Just a, 0) else (Nothing, s + b)
        findResult _ (Just res, _) = (Just res, 0)

data T = A | B | C deriving (Show, Eq)
testFreq = FreqDist [(A, 5), (B, 5), (C, 5)] :: FreqDist T Int
runFreqs g = res2
    where
        (res2, _) = getFreq g' testFreq
