module UI.App where
import GHC.IO.Handle
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty
import Data.Maybe (isJust, fromJust)
import Data.List (find)

import Data.TCPEvent
import Data.Entity
import Data.Default


data State = State
  { stateTCPHandle :: Handle
  , stateTerrain :: [String]
  , stateEntities :: [Entity]
  , stateLog :: [String]
  , statePlayer :: Entity
  }

data Resource = Resource ()
  deriving (Eq, Ord)


app :: App State TCPEvent Resource
app = App
  { appDraw = draw
  , appChooseCursor = const $ const Nothing
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap Graphics.Vty.defAttr []
  }

initialState :: Handle -> State
initialState handle = State
  { stateTCPHandle = handle
  , stateTerrain = ["Loading..."]
  , stateEntities = []
  , stateLog = ["If the log is empty, UI breaks."]
  , statePlayer = def
  }


draw :: State -> [Widget Resource]
draw s = [withBorderStyle unicodeRounded $ (drawMap s) <+> (drawStatus s)]

drawMap :: State -> Widget Resource
drawMap s = padRight Max $ str (unlines (stateTerrain s))

drawStatus :: State -> Widget Resource
drawStatus s = (drawPlayerInfo s) <=> (drawLog (stateLog s))

drawPlayerInfo :: State -> Widget Resource
drawPlayerInfo s = borderWithLabel (str (entityName playerConf)) $ hLimit 40 $ padRight Max $
  str $ "HP : " ++ show (entityHp playerState) ++ " / " ++ show (entityMaxHp playerConf) ++ "\n" ++
    "Wielding " ++ (maybe "nothing" id weaponName)
  where playerState = entityState (statePlayer s)
        playerConf = entityConf (statePlayer s)
        wielding = (entityWielding (entityState (statePlayer s)))
        weaponName = (entityName . entityConf) <$> (wielding >>= (\eid -> findEntity eid s))


drawLog :: [String] -> Widget Resource
drawLog l = borderWithLabel (str "Log") $ padBottom Max $ hLimit 40 $ padRight Max $
  (str . unlines) l

findEntity :: EntityId -> State -> Maybe Entity
findEntity e s = find (\e' -> entityId e' == e) (stateEntities s)


handleEvent :: State -> BrickEvent Resource TCPEvent -> EventM Resource (Next State)
handleEvent s (VtyEvent e) = handleVtyEvent s e
handleEvent s (AppEvent e) = handleAppEvent s e

handleVtyEvent :: State -> Event -> EventM Resource (Next State)
handleVtyEvent s (EvKey (KChar 'd') [MCtrl]) = halt s
handleVtyEvent s (EvKey k mod) = continue s
handleVtyEvent s (EvResize k y) = continue s
handleVtyEvent s _ = continue s

handleAppEvent :: State -> TCPEvent -> EventM Resource (Next State)
handleAppEvent s (TCPEventFail msg) =
  continue (s {stateLog = ("Failed to parse TCP message: " ++ msg):(stateLog s)})
handleAppEvent s (TCPEventLog msg) =
  continue (s {stateLog = msg:(stateLog s)})
handleAppEvent s (TCPEventTerrain t) =
  continue (s {stateTerrain = t})
handleAppEvent s (TCPEventEntityAdd e) =
  continue (s {stateEntities = e:(stateEntities s)})
handleAppEvent s (TCPEventEntityRemove eid) =
  continue (s {stateEntities = filter (\e -> entityId e /= eid) (stateEntities s)})
handleAppEvent s (TCPEventPlayerId eid) =
  if isJust player then
    continue (s {statePlayer = fromJust player})
  else
    handleAppEvent s (TCPEventFail $ "Failed to find player entity with Id " ++ show eid)
  where player = findEntity eid s
