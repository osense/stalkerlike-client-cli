{-# LANGUAGE DeriveGeneric #-}

module Data.Entity where
import Data.Default
import GHC.Generics
import Data.Aeson

type Id = Int
type Name = String

data Pos = Pos
  { posX :: Int
  , posY :: Int
  } deriving (Generic, Show)

type Rot = Int


data State = State
  { stateName :: Name
  , stateIcon :: Char
  , statePos :: Pos
  , stateRot :: Rot
  , stateHP :: Int
  , stateMaxHP :: Int
  } deriving (Generic, Show)

instance Default State where
  def = (State "Unknown" '?' (Pos 0 0) 0 100 100)

data MovesMode = Walk | Run | Crouch
  deriving (Generic, Show)

data Moves = Moves
  { movesWalkSpeed :: Int
  , movesRunSpeed :: Int
  , movesCrouchSpeed :: Int
  , movesModes :: [MovesMode]
  } deriving (Generic, Show)

data Item = Item
  { itemValue :: Int
  , itemWeight :: Double
  } deriving (Generic, Show)

data FireMode = Auto | Single | Burst
  deriving (Generic, Show)

data Weapon = Weapon
  { weaponAmmo :: [Name]
  , weaponMags :: [Name]
  , weaponAccuracy :: Double
  , weaponFireModes :: [FireMode]
  , weaponUpgrades :: [Name]
  , weaponMeleeDamage :: Int
  } deriving (Generic, Show)

data Ammo = Ammo
  { ammoDamage :: Int
  } deriving (Generic, Show)

data WeaponUpgrade = WeaponUpgrade
  { weaponUpgradeAmmo :: [Name]
  , weaponUpgradeAccuracy :: Double
  , weaponUpgradeFireModes :: [FireMode]
  , weaponUpgradeMeleeDamage :: Int
  } deriving (Generic, Show)

data Mag = Mag
  { capacity :: Int
  } deriving (Generic, Show)

data Component =
    CompMoves Moves
  | CompItem Item
  | CompWeapon Weapon
  | CompAmmo Ammo
  | CompWeaponUpgrade WeaponUpgrade
  | CompMag Mag
    deriving (Generic, Show)

data Entity = Entity
  { entityId :: Int
  , entityState :: State
  , comps :: [Component]
  }
  deriving (Generic, Show)

instance Default Entity where
  def = (Entity 0 def [])


instance FromJSON Pos
instance FromJSON State
instance FromJSON MovesMode
instance FromJSON Moves
instance FromJSON Item
instance FromJSON FireMode
instance FromJSON Weapon
instance FromJSON Ammo
instance FromJSON WeaponUpgrade
instance FromJSON Mag
instance FromJSON Component
instance FromJSON Entity

instance ToJSON Pos
instance ToJSON State
instance ToJSON MovesMode
instance ToJSON Moves
instance ToJSON Item
instance ToJSON FireMode
instance ToJSON Weapon
instance ToJSON Ammo
instance ToJSON WeaponUpgrade
instance ToJSON Mag
instance ToJSON Component
instance ToJSON Entity
