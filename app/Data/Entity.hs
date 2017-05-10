{-# LANGUAGE DeriveGeneric #-}

module Data.Entity where
import Data.Default
import GHC.Generics
import Data.Aeson

type EntityId = Int
type EntityName = String

data Pos = Pos
  { posX :: Int
  , posY :: Int
  } deriving (Generic, Show)

type Rot = Int

data Bearing = Standing | Crouching
  deriving (Generic, Show)

data Action = Walk | Run | Crouch | PickUpItem | EquipItem | ConsumeItem | Attack | PickUp
  deriving (Generic, Show)

data EntityState = EntityState
  { entityPos :: Pos
  , entityRot :: Rot
  , entityBearing :: Bearing
  , entityHP :: Int
  , entityWielding :: Maybe EntityId
  } deriving (Generic, Show)

data EntityConf = EntityConf
  { entityName :: EntityName
  , entityIcon :: Char
  , entityMaxHP :: Int
  , entityActions :: [Action]
  , entityMore :: [More]
  } deriving (Generic, Show)


data FireMode = Single | Burst | Full
  deriving (Generic, Show)

data Alive = Alive
  { aliveWalkSpeed :: Int
  , aliveRunSpeed :: Int
  , aliveMinAttack :: Int
  , aliveMaxAttack :: Int
  } deriving (Generic, Show)

data Weapon = Weapon
  { weaponAmmo :: [EntityName]
  , weaponMags :: [EntityName]
  , weaponAccuracy :: Int
  , weaponFireModes :: [FireMode]
  , weaponUpgrades :: [EntityName]
  , weaponMeleeDamage :: Int
  } deriving (Generic, Show)

data Ammo = Ammo
  { ammoDamage :: Int
  } deriving (Generic, Show)

data WeaponUpgrade = WeaponUpgrade
  { weaponUpgradeAmmo :: [EntityName]
  , weaponUpgradeAccuracy :: Int
  , weaponUpgradeFireModes :: [FireMode]
  , weaponUpgradeMeleeDamage :: Int
  } deriving (Generic, Show)

data Mag = Mag
  { capacity :: Int
  } deriving (Generic, Show)

data More = MoreAlive Alive | MoreWeapon Weapon | MoreAmmo Ammo | MoreWeaponUpgrade WeaponUpgrade | MoreMag Mag
  deriving (Generic, Show)

data Entity = Entity
  { entityId :: EntityId
  , entityState :: EntityState
  , entityConf :: EntityConf
  } deriving (Generic, Show)


instance Default EntityState where
  def = EntityState
    { entityPos = Pos {posX = 0, posY = 0}
    , entityRot = 0
    , entityBearing = Standing
    , entityHp = 100
    , entityWielding = Nothing
    }

instance Default EntityConf where
  def = EntityConf
    { entityName = "Unknown"
    , entityIcon = 'X'
    , entityMaxHp = 100
    , entityActions = []
    , entityMore = []
    }

instance Default Entity where
  def = Entity 0 def def


instance FromJSON Pos
instance ToJSON Pos
instance FromJSON Bearing
instance ToJSON Bearing
instance FromJSON Action
instance ToJSON Action
instance FromJSON EntityState
instance ToJSON EntityState
instance FromJSON EntityConf
instance ToJSON EntityConf
instance FromJSON FireMode
instance ToJSON FireMode
instance FromJSON Alive
instance ToJSON Alive
instance FromJSON Weapon
instance ToJSON Weapon
instance FromJSON Ammo
instance ToJSON Ammo
instance FromJSON WeaponUpgrade
instance ToJSON WeaponUpgrade
instance FromJSON Mag
instance ToJSON Mag
instance FromJSON More
instance ToJSON More
instance FromJSON Entity
instance ToJSON Entity
