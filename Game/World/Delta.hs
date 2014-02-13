{-# LANGUAGE FlexibleContexts, TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
module Game.World.Delta where

import qualified Data.Map as Map
import Control.Lens
import Game.World.Objects
import Data.Monoid
import Control.Monad.Writer
import Control.Monad.RWS
import Linear
--import Game.World.Types
import Game.Collision
import Control.Monad.State
import Control.Monad
import qualified Control.Wire as W
import Data.Maybe
import Game.World.Common
import Game.World.Lens

--applyCommonDelta :: State World ()
applyCommonDelta wd = do
	-- add delta to last position or insert new
	wCommon.wcPositions %= \positions ->
		foldr (\(k, v) -> Map.alter (alterPos v) k) positions $ 
			Map.toList (wd^.wdCommon.delta.wcPositions)

	-- take the latest animations
	wCommon.wcAnimations %= \animations -> 
		(wd^.wdCommon.delta.wcAnimations) `Map.union` animations -- left biased

	-- we drop all wires from the last state
	wCommon.wcWires .= wd^.wdCommon.delta.wcWires

alterObjects delta@Nothing _ = Nothing -- delte object
alterObjects (Just v) _ = Just v -- add / update object

applyObjectDelta wd = do
	wObjects %= \objects -> 
		foldr (\(k, v) -> Map.alter (alterObjects v) k) objects $
			Map.toList (wd^.wdObjects)

applyDelta :: World -> WorldDelta -> World
applyDelta w wd = execState (do
		applyObjectDelta wd
		applyCommonDelta wd
	) w
