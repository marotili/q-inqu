module Game.Input.Actions where

import Data.Binary
import Data.Monoid
import Linear

data Direction = 
	  DirNorth
	| DirEast
	| DirSouth
	| DirWest
	deriving (Show, Eq)

data Action = 
	  ActionNothing
	| ActionMove Float Float -- normalized direction
	| ActionPickup
	| ActionActivate Direction
	deriving (Show, Eq)

newMoveAction :: Float -> Float -> Action
newMoveAction x y = ActionMove x' y'
	where
		V2 x' y' = normalize $ V2 x y

-- output of wire
newtype InputActions = InputActions [Action]
	deriving (Show)

newInputAction :: Action -> InputActions
newInputAction action = InputActions [action]

instance Monoid InputActions where
	mempty = InputActions []
	--mappend :: InputActions -> InputActions -> InputActions
	mappend (InputActions []) as = as
	mappend as (InputActions []) = as

	mappend (InputActions [action@ActionNothing]) (InputActions as) = InputActions $
		if action `elem` as then as else action:as

	-- Add debug infos, two input actions should not happen
	mappend (InputActions [action@(ActionActivate dir)]) (InputActions as) = InputActions $ 
		if action `elem` as then as else action:as

	mappend (InputActions [action@ActionPickup]) (InputActions as) = InputActions $
		if action `elem` as then as else action:as

	mappend (InputActions [ActionMove x y]) (InputActions as) = InputActions $
		foldr (\a ls -> case a of 
			ActionMove x' y' -> ActionMove (x+x') (y+y'):ls
			_ -> a:ls
			) [] as


	mappend as1 (InputActions as2) = foldr (\a -> mappend (InputActions [a])) as1 as2

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

instance Binary Action where
	put ActionNothing = put (0 :: Word8)
	put (ActionMove x y) = do
		put (1 :: Word8)
		put x
		put y
	put (ActionActivate dir) = do
		put (2 :: Word8)
		put dir

	get = do
		t <- get :: Get Word8
		case t of 
			0 -> return ActionNothing
			1 -> do
				x <- get
				y <- get
				return $ ActionMove x y
			2 -> do
				dir <- get
				return $ ActionActivate dir
