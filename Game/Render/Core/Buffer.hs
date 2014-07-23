{-# LANGUAGE TemplateHaskell, Rank2Types, ImpredicativeTypes #-}
module Game.Render.Core.Buffer
()
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Storable as V

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.ST

import Control.Lens
import Data.Word

-- | utils to manage buffers etc.

type BufferName = Int

data BufferManager = BufferManager
    { _bmBuffers :: Set.Set BufferName
    , _bmBufferData :: Map.Map BufferName (V.Vector Word32)
    }
makeLenses ''BufferManager

-- | create a new (empty) manager
newBufferManager = BufferManager Set.empty Map.empty

-- | TODO: remove BufferManager as first parameter
-- | modify a single value inside the buffer given the name and the index
modifyBuffer :: BufferManager -> BufferName -> Int -> Word32 -> State BufferManager ()
modifyBuffer bm bufferName index value =
    let Just buffer = bm^.bmBufferData.at bufferName
        buf2 = runST $ do
            modBuf <- V.unsafeThaw buffer
            VM.write modBuf index value
            V.unsafeFreeze modBuf
    in bmBufferData %= Map.insert bufferName buf2

-- | read a single value out of the buffer
readBuffer :: BufferName -> Int -> State BufferManager Word32
readBuffer bufferName index = do
    Just buffer <- use $ bmBufferData . at bufferName
    return $ buffer V.! index

-- | create a new buffer with given name
addBuffer :: BufferName -> State BufferManager ()
addBuffer bufferName = 
    let buffer = runST $ do
                    vector <- VM.new 100
                    V.unsafeFreeze vector
    in do
        bmBuffers %= Set.insert bufferName
        bmBufferData %= (Map.insert bufferName buffer)

test = do
    let bm = newBufferManager
    let bm' = execState (
                do
                    addBuffer 0
                    bm <- get
                    modifyBuffer bm 0 1 8
            ) bm

    print $ evalState (
        readBuffer 0 1
        ) bm'


