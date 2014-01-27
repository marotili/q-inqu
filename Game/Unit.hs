{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, FlexibleContexts, Arrows, NoMonomorphismRestriction,
	FlexibleInstances, Rank2Types, RankNTypes #-}
module Game.Unit where

import Control.Monad.Identity (Identity)
import Control.Monad.State hiding (when)
import Control.Wire ((.))
import Control.Wire
--import Control.Monoid
import qualified Control.Wire as W
import Prelude hiding ((.), until, when)
import Control.Monad.Reader hiding (when)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Arrow

import Control.Wire.Unsafe.Event

import qualified Graphics.UI.GLFW as GLFW

data UnitType = LightInfantry
	{ unitArmor :: Integer
	, unitStartHealth :: Integer
	} | HeavyInfantry
	{ unitArmor :: Integer
	, unitStartHealth :: Integer
	}  deriving (Eq, Ord)

data UnitInstance = UnitInstance
	{ unitInstanceType :: UnitType
	, unitHealth :: Integer
	} deriving (Eq, Ord)

type AttackPower = Integer
type DefensePower = Integer
type AttackModifier = AttackPower -> AttackPower

scale :: (RealFrac a, Integral b) => a -> b -> b
scale a = round . (*) (a) . fromIntegral

ignore1Arg :: a -> (b -> a)
ignore1Arg f = (\_ -> f)
ignore2Args :: a -> (b -> c -> a)
ignore2Args f = ignore1Arg . ignore1Arg $ f

-- the attack power of a unit is relative to its remaining health
unitInstanceAttackPower :: UnitInstance -> AttackPower
unitInstanceAttackPower unit@UnitInstance{unitInstanceType} = unitAttackPower unitInstanceType unit

unitAttackPower :: UnitType -> (UnitInstance -> AttackPower)
unitAttackPower LightInfantry {} = (\UnitInstance { unitHealth } -> unitHealth)
unitAttackPower HeavyInfantry { unitStartHealth } = (\UnitInstance { unitHealth } -> unitStartHealth * (unitHealth `div` unitStartHealth) ^ (2 :: Integer))

unitInstanceAttackModifier :: UnitInstance -> UnitInstance -> AttackModifier
unitInstanceAttackModifier
	attackUnit@UnitInstance{unitInstanceType=attackUnitType}
	defendUnit@UnitInstance{unitInstanceType=defendUnitType}
	= unitAttackModifier attackUnitType defendUnitType attackUnit defendUnit


unitAttackModifier :: UnitType -> UnitType -> (UnitInstance -> UnitInstance -> AttackModifier)
unitAttackModifier LightInfantry {} LightInfantry {} = ignore2Args $ (+0)
unitAttackModifier LightInfantry {} HeavyInfantry {} = ignore2Args $ scale (0.8::Double)
unitAttackModifier HeavyInfantry {} LightInfantry {} = ignore2Args $ scale (1.2::Double)

unitInstancePerformAttack :: UnitInstance -> UnitInstance -> AttackPower
unitInstancePerformAttack attackUnit defendUnit = attackMod attackPower
	where
		attackPower = unitInstanceAttackPower attackUnit
		attackMod = unitInstanceAttackModifier attackUnit defendUnit

newLightInfantry :: UnitInstance
newLightInfantry = UnitInstance
	{ unitInstanceType = LightInfantry
		{ unitArmor = 10,
		  unitStartHealth
		}
	, unitHealth = unitStartHealth
	}
	where
		unitStartHealth = 10

data World = World
	{ worldCells :: Map.Map (Int, Int) Cell
	, worldUnits :: Set.Set UnitInstance
	} deriving (Eq)

worldNew :: World
worldNew = World
	{ worldCells = foldr (\k -> Map.insert k newCell) Map.empty [(x, y) | x <- [1..16], y <- [1..16]]
	, worldUnits = Set.empty
	}

worldUpdateCell :: (Cell -> Cell) -> (Int, Int) -> World -> World
worldUpdateCell f pos world= world { worldCells = Map.adjust f pos (worldCells world) }

worldCellAt :: (Int, Int) -> World -> Maybe Cell
worldCellAt pos world = Map.lookup pos (worldCells world)

worldSpawnUnit :: (Int, Int) -> UnitInstance -> World -> World
worldSpawnUnit pos unit = proc world -> do
	w <- worldUpdateCell (cellSetUnit unit) pos -< world
	w1 <- worldAddUnit unit -< w
	returnA -< w1
	where
		worldAddUnit u w = w { worldUnits = Set.insert u (worldUnits w)}

data WorldFailure a =
	  FailureNoUnit
	| FailureSpaceNotEmpty
	| NoFailure World a

(>>>>) :: WorldFailure a -> (a -> World -> WorldFailure b) -> WorldFailure b
(>>>>) (NoFailure w a) f = f a w
(>>>>) FailureNoUnit _ = FailureNoUnit
(>>>>) FailureSpaceNotEmpty _ = FailureSpaceNotEmpty

worldNeedUnitAt :: (Int, Int) -> World -> WorldFailure UnitInstance
worldNeedUnitAt pos world =
	case unitAt of
		Nothing -> FailureNoUnit
		Just unit -> NoFailure world unit
	where
		unitAt = worldCellAt pos world >>= cellUnit

worldNeedEmptySpace :: (Int, Int) -> World -> WorldFailure ()
worldNeedEmptySpace pos world =
	case unitAt of
		Nothing -> NoFailure world ()
		Just unit -> FailureSpaceNotEmpty
	where
		unitAt = worldCellAt pos world >>= cellUnit

worldMoveUnit :: (Int, Int) -> (Int, Int) -> World -> WorldFailure ()
worldMoveUnit from to world = worldNeedEmptySpace to world
		>>>> (\_ -> worldNeedUnitAt from) >>>>
	(\unit w -> NoFailure (worldUpdateCell cellUnsetUnit from >>> worldUpdateCell (cellSetUnit unit) to $ w) ())

data Cell = Cell
	{ cellUnit :: Maybe UnitInstance
	} deriving (Eq)

newCell :: Cell
newCell = Cell
	{ cellUnit = Nothing
	}

cellHasUnit :: Cell -> Bool
cellHasUnit = not . cellHasNoUnit

cellHasNoUnit :: Cell -> Bool
cellHasNoUnit Cell { cellUnit = Nothing } = True
cellHasNoUnit _ = False

cellSetUnit :: UnitInstance -> Cell -> Cell
cellSetUnit unit cell = cell { cellUnit = Just unit }

cellUnsetUnit :: Cell -> Cell
cellUnsetUnit cell = cell { cellUnit = Nothing }
