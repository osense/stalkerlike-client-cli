{-# LANGUAGE DeriveGeneric #-}

module Data.TCPEvent where
import GHC.Generics
import Data.Aeson
import Data.Entity


type Terrain = [String]

data TCPEvent =
    TCPEventFail String
  | TCPEventLog String
  | TCPEventTerrain Terrain
  | TCPEventEntityAdd Entity
  | TCPEventEntityRemove EntityId
  | TCPEventPlayerId EntityId
  deriving (Generic, Show)

instance FromJSON TCPEvent
instance ToJSON TCPEvent
