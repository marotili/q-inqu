{-# LANGUAGE Arrows, NamedFieldPuns #-}
module Game.Objects where
import qualified Data.Set as Set
import qualified Data.Binary as B

type ObjectIds = Set.Set ObjectId
type ObjectId = Int

type DoorId = ObjectId
type DoorControllerId = ObjectId
type SwitchId = ObjectId
type PlayerId = ObjectId
type WallId = ObjectId

data Wall = Wall 
	{ wallId :: WallId
	} deriving (Show, Eq)

data Player = Player
	{ playerId :: PlayerId
	, playerName :: String
	} deriving (Show, Eq)

data Door = Door
	{ doorId :: DoorId
	, doorOpen :: Bool
	} deriving (Show, Eq)
--newDoor :: DoorId -> Door
--newDoor doorId = Door doorId False

data DoorController = DoorController
	{ doorControllerId :: DoorControllerId
	, dcTargetDoorId :: DoorId
	, dcTimeRunning :: Float
	, dcTimeNeedsToOpen :: Float
	, dcStarted :: Bool
	} deriving (Show, Eq)
--newDoorController :: DoorId -> DoorControllerId -> DoorController
--newDoorController doorId dcId = DoorController dcId doorId 0 1 False

data Switch = Switch
	{ switchId :: SwitchId
	, switchOn :: Bool
	} deriving (Show, Eq)


instance B.Binary Player where
	put (Player {playerId, playerName}) = do
		B.put playerId
		B.put playerName
	get = do
		pId <- B.get
		pName <- B.get
		return (Player pId pName)

instance B.Binary Door where
	put (Door { doorId, doorOpen }) = B.put doorId >> B.put doorOpen
	get = do
		dId <- B.get
		dOpen <- B.get
		return Door { doorId = dId, doorOpen = dOpen}

instance B.Binary DoorController where
	put DoorController 
		{ doorControllerId
		, dcTargetDoorId
		, dcTimeRunning
		, dcTimeNeedsToOpen
		, dcStarted
		} = do
			B.put doorControllerId
			B.put dcTargetDoorId
			B.put dcTimeRunning
			B.put dcTimeNeedsToOpen
			B.put dcStarted

	get = do
		dcId <- B.get
		dcTDId <- B.get
		dcTR <- B.get
		dcTNTO <- B.get
		dcS <- B.get

		return DoorController 
			{ doorControllerId = dcId
			, dcTargetDoorId = dcTDId
			, dcTimeRunning = dcTR
			, dcTimeNeedsToOpen = dcTNTO
			, dcStarted = dcS
			}

instance B.Binary Switch where
	put Switch
		{ switchId
		, switchOn
		} = B.put switchId >> B.put switchOn

	get = do
		sId <- B.get
		sOn <- B.get
		return Switch { switchId = sId, switchOn = sOn }

