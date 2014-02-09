{-# LANGUAGE  TemplateHaskell, NamedFieldPuns, Rank2Types #-}
module Game.World.Types
    (
    -- * World
      World(..)

    , wPlayerId, wPlayerPos
    , wBoulderId, wBoulderPos
    , wPhysics, wCollisionManager
    , wBoulders, wPositions
    , wObjectSpeed, wObjectAccel
    , wCurrentCollisions
    , wTileBoundary

    , newWorld
    ) where

import Control.Lens
import qualified Data.Map as Map
import Game.World.Objects
import Game.Collision
import Data.Maybe

data World = World
    { _wDoors :: Map.Map DoorId Door
    , _wBoulders :: Map.Map BoulderId Boulder
    , _wWalls :: Map.Map WallId Wall
    , _wDoorControllers :: Map.Map DoorControllerId DoorController
    , _wSwitches :: Map.Map SwitchId Switch
    , _wPositions :: Map.Map ObjectId (Float, Float)
    , _wPhysics :: Map.Map ObjectId ObjectPhysics
    , _wPlayers :: Map.Map ObjectId Player
    , _wCollisionManager :: CollisionManager
    , _wCurrentCollisions :: Map.Map ObjectId [ObjectId]
    , _wTileBoundary :: (Float, Float)
    } deriving (Eq)

makeLenses ''World

newWorld = World
    { _wDoors = Map.empty
    , _wWalls = Map.empty
    , _wBoulders = Map.empty
    , _wDoorControllers = Map.empty
    , _wSwitches = Map.empty
    , _wPositions = Map.empty
    , _wPhysics = Map.empty
    , _wPlayers = Map.empty
    , _wCollisionManager = cmNew
    , _wCurrentCollisions = Map.empty
    , _wTileBoundary = (0, 0)
    --, wMap = gameMap
    }

wPlayerId :: String -> Getter World (Maybe PlayerId)
wPlayerId playerName' = to (\w -> case getPlayer w playerName' of
            [pId] -> Just pId
            _ -> Nothing
        )
    where
        getPlayer world playerName' = ifoldMap (\pId Player { playerName } ->
                [pId | playerName == playerName']
            ) (world^.wPlayers)

wBoulderId :: String -> Getter World (Maybe BoulderId)
wBoulderId boulderName' = to (\w -> case getBoulder w boulderName' of
        [bId] -> Just bId
        _ -> Nothing
    )

getBoulder :: World -> String -> [BoulderId]
getBoulder w name = getObject w wBoulders ((==name) . boulderName)

getObject :: World -> Lens' World (Map.Map k a) -> (a -> Bool) -> [k]
getObject w g cond = ifoldMap (\oId object ->
        [oId | cond object]
        ) (w^.g)

wObjectPos :: Getter World (Maybe PlayerId) -> Getter World (Maybe (Float, Float))
wObjectPos oId = to (\w -> case w^.oId of
        Just oId -> w^.wPositions . at oId
        Nothing -> Nothing
    )

wObjectSpeed :: ObjectId -> Getter World (Float, Float)
wObjectSpeed oId = to (\w -> w^.wPhysics . at oId ^?! _Just . objectSpeed . tuple)

wObjectAccel :: ObjectId -> Getter World (Float, Float)
wObjectAccel oId = to (\w -> w^.wPhysics . at oId ^?! _Just . objectAcceleration . tuple)

--wObjectPos w oId = w^.wPositions . at (w^.oId)

wBoulderPos :: String -> Getter World (Maybe (Float, Float))
wBoulderPos name = wObjectPos $ wBoulderId name

wPlayerPos :: String -> Getter World (Maybe (Float, Float))
wPlayerPos playerName' = to (\w -> case w^.pId of
            Just _ -> w^.wPositions . at (fromJust $ w^.pId)
            Nothing -> Nothing
        )
    where
        pId = wPlayerId playerName'
