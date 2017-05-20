{-# LANGUAGE DeriveGeneric #-}

module Data.TCP where
import GHC.Generics
import Data.Aeson
import Data.Entity


data Tile = Tile
  { tileType :: Char
  , tileMoves :: Int
  , tilePassable :: Bool
  , tileVision :: Bool
  }

type Terrain = [String]

data Event =
    EventLoginMOTD String
  | EventLoginSuccess
  | EventFail String
  | EventLog String
  | EventTerrain Terrain
  | EventEntityAdd Entity
  | EventEntityRemove Id
  | EventPlayerId Id
    deriving (Generic, Show)

instance FromJSON Event


data Dir = DirUp | DirDown | DirLeft | DirRight | DirUpLeft | DirUpRight | DirDownLeft | DirDownRight
  deriving (Eq, Generic, Show)

data Command =
    CommandLogin String String
  | CommandExit
  | CommandMove Dir
    deriving (Eq, Generic, Show)

instance ToJSON Dir
instance ToJSON Command
