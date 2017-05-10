module UI.App where
import Network.Connection (Connection)
import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty
import Data.Maybe (isJust, fromJust)
import Data.List (find)

import qualified Data.TCP as TCP
import Data.Entity
import Data.Default


data State = State
  { stateCon :: Connection
  , stateTerrain :: [String]
  , stateEntities :: [Entity]
  , stateLog :: [String]
  , statePlayer :: Entity
  }

data Resource = Resource ()
  deriving (Eq, Ord)


runApp :: Connection -> BChan TCP.Event -> IO State
runApp con chan = customMain
  (Graphics.Vty.mkVty Graphics.Vty.defaultConfig)
  (Just chan) app (initialState con)

app :: App State TCP.Event Resource
app = App
  { appDraw = draw
  , appChooseCursor = const $ const Nothing
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap Graphics.Vty.defAttr []
  }

initialState :: Connection -> State
initialState con = State
  { stateCon = con
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


handleEvent :: State -> BrickEvent Resource TCP.Event -> EventM Resource (Next State)
handleEvent s (VtyEvent e) = handleVtyEvent s e
handleEvent s (AppEvent e) = handleAppEvent s e

handleVtyEvent :: State -> Event -> EventM Resource (Next State)
handleVtyEvent s (EvKey (KChar 'd') [MCtrl]) = halt s
handleVtyEvent s (EvKey k mod) = continue s
handleVtyEvent s (EvResize k y) = continue s
handleVtyEvent s _ = continue s

handleAppEvent :: State -> TCP.Event -> EventM Resource (Next State)
handleAppEvent s (TCP.EventFail msg) =
  continue (s {stateLog = ("Failed to parse TCP. message: " ++ msg):(stateLog s)})
handleAppEvent s (TCP.EventLog msg) =
  continue (s {stateLog = msg:(stateLog s)})
handleAppEvent s (TCP.EventTerrain t) =
  continue (s {stateTerrain = t})
handleAppEvent s (TCP.EventEntityAdd e) =
  continue (s {stateEntities = e:(stateEntities s)})
handleAppEvent s (TCP.EventEntityRemove eid) =
  continue (s {stateEntities = filter (\e -> entityId e /= eid) (stateEntities s)})
handleAppEvent s (TCP.EventPlayerId eid) =
  if isJust player then
    continue (s {statePlayer = fromJust player})
  else
    handleAppEvent s (TCP.EventFail $ "Failed to find player entity with Id " ++ show eid)
  where player = findEntity eid s
