module LogoProtocol where

data Shard
  = S
  | E
  | N
  | W
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

type Pos = (Int, Int)

type TileData = [(Pos, [Shard])]

data Tile = Tile
  { _tilePos :: Pos
  , _tileData :: TileData
  } deriving (Show)

data St
  = Selecting
      { _selected :: Tile
      , _notSelected :: [Tile]
      }
  | Dragging
      { _dragged :: Tile
      , _inactive :: [Tile]
      }
  deriving (Show)

type CoverageInfo = (Int, Int, Shard, Bool)

data InMsg
  = Poll
  | Cover [(Int, Int, Shard)]
  | Quit
  deriving (Show, Read)

data OutMsg
  = Stats [CoverageInfo]
  | Error
  deriving (Show, Read)
