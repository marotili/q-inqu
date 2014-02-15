{-# LANGUAGE NamedFieldPuns #-}
module Game.World.Gen.Frequency where

import qualified Data.Map as Map
import System.Random
import Data.Maybe
import Debug.Trace
import Test.QuickCheck
import Game.World.Gen.Types
import Control.Monad.State
import Control.Lens

data FreqDist a b = FreqDist
    { freqDist :: [(a, b)]
    }

getDistribution :: (Show b, Num b, Ord b, Random b) 
    => FreqDist a b -> GenContext a
getDistribution freq = do
    g <- use stateRndGen
    let (res, g') = _getDistribution g freq
    stateRndGen .= g'
    return res

_getDistribution :: (Show b, Num b, Ord b, Random b, RandomGen g) 
    => g -> FreqDist a b -> (a, g)
_getDistribution g FreqDist { freqDist } = (result, g')
    where
        range = (0, (sum . map snd $ freqDist) - 1)
        (randVal, g') = randomR range g
        result = fromJust . fst $ 
            foldr findResult (Nothing, 0) freqDist

        findResult (a, b) (Nothing, s) = 
            if s + b > randVal 
                then (Just a, 0) 
                else (Nothing, s + b)
        findResult _ (Just res, _) = (Just res, 0)

roomTypeFreqDist = FreqDist 
    [ (DefaultRoom, 9 :: Int)
    , (BossRoom, 1)
    ]

roomSizeFreqDist DefaultRoom = FreqDist
    [ (RoomSize 2 2, 1 :: Int)
    , (RoomSize 2 3, 1)
    , (RoomSize 2 4, 1)
    , (RoomSize 3 2, 1)
    , (RoomSize 3 3, 1)
    , (RoomSize 3 4, 1)
    , (RoomSize 4 2, 1)
    , (RoomSize 4 3, 1)
    , (RoomSize 4 4, 1)
    ]

-- | Boss rooms are always big
roomSizeFreqDist BossRoom = FreqDist
    [ (RoomSize 4 4, 1)
    ]

initTest =
    newStdGen


