{-# LANGUAGE NamedFieldPuns #-}
module Game.World.Gen where

import Control.Lens
import Game.World.Gen.Types
import Game.World.Gen.Frequency

corridorVectors :: Corridor -> [(Int, Int)]
corridorVectors corridor = vectors
    where
        points = corridor^.corPoints
        corridorLines = zip points (tail points)
        vectors = map (\((x1, y1), (x2, y2)) -> 
            (x2 - x1, y2 - y1)) corridorLines

validateCorridor :: Corridor -> Bool
validateCorridor corridor = valid
    where
        vectors = corridorVectors corridor
        -- the corridor lines must be horizontal or vertical
        valid = all (\(x, y) -> x == 0 || y == 0) vectors

-- | uses manhattan metric
corridorLength :: Corridor -> Int
corridorLength corridor = sum . map (uncurry (+)) $ corridorVectors corridor

roomDoorsAreOnOppositeSides :: Room -> DoorIndex -> DoorIndex -> Bool
roomDoorsAreOnOppositeSides room dIdx1 dIdx2 = ret
    where
        Just (dx1, dy1) = room^?roomDoors.at dIdx1._Just.doorPosition
        Just (dx2, dy2) = room^?roomDoors.at dIdx2._Just.doorPosition
        RoomSize w h = room^.roomSize

        leftx = min dx1 dx2
        topy = min dy1 dy2

        rightx = max dx1 dx2
        bottomy = max dy1 dy2

        ret = leftx + w == rightx || topy + h == bottomy

newRandomEmptyRoom :: GenContext Room
newRandomEmptyRoom = do
    rndRoomType <- getDistribution roomTypeFreqDist
    rndRoomSize <- getDistribution (roomSizeFreqDist rndRoomType)
    return $ newRoom
        & roomSize .~ rndRoomSize
        & roomType .~ rndRoomType

--runTest :: IO ()
--runTest = initTest >>= print . runGenContext genMap 
--roomTest =
    --initTest >>= print . runGenContext newRandomEmptyRoom

