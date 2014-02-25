module Game.Input.Actions 
	(
	-- * Actions
	  Action(..), Direction(..), InputActions(..)
	, newInputAction
	, movingDirection, newMoveAction
	) where

import Data.Binary
import Data.Monoid
import Linear
import qualified Data.Set as Set

data Direction = 
	  DirNorth
	| DirEast
	| DirSouth
	| DirWest
	deriving (Show, Eq, Ord)

data Action = 
	  ActionNothing
	| ActionMove Float Float -- normalized direction
	| ActionStopMove -- maybe needed for interpolation
	| ActionPickup
	| ActionActivate Direction
	| ActionSpawnArrow Float Float
	| ActionUpdateGameState
	deriving (Show, Eq, Ord)

--instants = 
--	[ ActionActivate DirNorth
--	, ActionSpawnArrow
--	, ActionStopMove
--	, ActionUpdateGameState
--	]



newMoveAction :: Float -> Float -> Action
newMoveAction x y = ActionMove x' y'
	where
		V2 x' y' = normalize $ V2 x y

-- output of wire
newtype InputActions = InputActions (Set.Set Action)
	deriving (Show, Eq)

newInputAction :: Action -> InputActions
newInputAction action = InputActions (Set.insert action Set.empty)

isMoving :: InputActions -> Bool
isMoving (InputActions actions) = 
	any (\a -> case a of ActionMove _ _ -> True; _ -> False) (Set.toList actions)

movingDirection :: InputActions -> (Float, Float)
movingDirection is@(InputActions actions) = if isMoving is 
	then foldr (\a (dx, dy) -> case a of ActionMove x y -> (x + dx, y + dy); _ -> (dx, dy)) (0, 0) (Set.toList actions)
	else (0, 0)

-- FIXME: there should be an easier solution
-- TODO: FIXME
-- Define user input (event actions / continuous actions)
-- * move actions
-- * switch actions
instance Monoid InputActions where
	mempty = InputActions Set.empty
	--mappend :: InputActions -> InputActions -> InputActions
	mappend (InputActions as1) (InputActions as2) = InputActions $ 
		foldr (\a as -> case a of
			ActionMove x y -> Set.insert (normMove x y x' y') (removeStops as)
			_ -> Set.insert a as
		) left (Set.toList as2)
		where
			(x', y') = movingDirection (InputActions as1)
			removeMovement as1' = foldr (\a as -> case a of
				ActionMove _ _ -> as
				_ -> Set.insert a as
				) Set.empty (Set.toList as1')

			normMove x1 y1 x2 y2 = ActionMove a b
				where
					V2 a b = normalize (V2 (x1 + x2) (y1 + y2))

			left = if isMoving (InputActions as2) || ActionStopMove `elem` Set.toList as2 then removeMovement as1 else as1

			removeStops as = foldr (\a as' -> case a of ActionStopMove -> as'; _ -> Set.insert a as') Set.empty (Set.toList as)

instance Binary Direction where
	put DirNorth = put (0 :: Word8)
	put DirEast = put (1 :: Word8)
	put DirSouth = put (2 :: Word8)
	put DirWest = put (3 :: Word8)

	get = do
		dir <- get :: Get Word8
		case dir of
			0 -> return DirNorth
			1 -> return DirEast
			2 -> return DirSouth
			3 -> return DirWest
			_ -> error "Invalid value while decoding Direction"

instance Binary Action where
	put ActionNothing = put (0 :: Word8)
	put (ActionMove x y) = do
		put (1 :: Word8)
		put x
		put y
	put (ActionActivate dir) = do
		put (2 :: Word8)
		put dir
	put ActionPickup = put (3 :: Word8)
	put ActionStopMove = put (4 :: Word8)
	put (ActionSpawnArrow x y) = do
		put (5 :: Word8)
		put x
		put y
	put ActionUpdateGameState = put (6 :: Word8)

	get = do
		t <- get :: Get Word8
		case t of 
			0 -> 
				return ActionNothing
			1 -> do
				x <- get
				y <- get
				return $ ActionMove x y
			2 -> do
				dir <- get
				return $ ActionActivate dir
			3 -> 
				return ActionPickup	
			4 -> 
				return ActionStopMove
			5 -> do
				x <- get
				y <- get
				return $ ActionSpawnArrow x y
			6 -> 
				return ActionUpdateGameState
			_ -> error "Invalid value while decoding Action"
