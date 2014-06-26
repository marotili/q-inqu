module Game.Console.Core () where

import Control.Lens

data Console = Console
    { _currentBuffer :: B.Bytestring
    }

type Command = String
type ByteOffset = Int

data CommandBuffer = CommandBuffer
    { _cbBuffer :: String
    , _cbCommands :: [(ByteOffset, Command)]
    }

