module UI.App where
import Control.Concurrent.STM.TMChan (TMChan)
import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty
import Data.Maybe (isJust, fromJust)
import Data.List (find)

import qualified Data.TCP as TCP
import Data.Entity (Entity)
import qualified Data.Entity as E
import Data.Default


data State = State
  { stateChan :: TMChan TCP.Command
  , stateTerrain :: [String]
  , stateEntities :: [Entity]
  , stateLog :: [String]
  , statePlayer :: Entity
  }

data Resource = Resource ()
  deriving (Eq, Ord)


runApp :: BChan TCP.Event -> TMChan TCP.Command -> IO State
runApp downChan upChan = customMain
  (Graphics.Vty.mkVty Graphics.Vty.defaultConfig)
  (Just downChan) app (initialState upChan)

app :: App State TCP.Event Resource
app = App
  { appDraw = draw
  , appChooseCursor = const $ const Nothing
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap Graphics.Vty.defAttr []
  }

initialState :: TMChan TCP.Command -> State
initialState chan = State
  { stateChan = chan
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
drawPlayerInfo s = borderWithLabel (str (E.stateName playerState)) $ hLimit 40 $ padRight Max $
  str $ "HP : " ++ show (E.stateHP playerState) ++ " / " ++ show (E.stateMaxHP playerState)
  where playerState = E.entityState (statePlayer s)


drawLog :: [String] -> Widget Resource
drawLog l = borderWithLabel (str "Log") $ padBottom Max $ hLimit 40 $ padRight Max $
  (str . unlines) l

findEntity :: E.Id -> State -> Maybe Entity
findEntity e s = find (\e' -> E.entityId e' == e) (stateEntities s)


handleEvent :: State -> BrickEvent Resource TCP.Event -> EventM Resource (Next State)
handleEvent s (VtyEvent e) = handleVtyEvent s e
handleEvent s (AppEvent e) = handleAppEvent s e

handleVtyEvent :: State -> Event -> EventM Resource (Next State)
handleVtyEvent s (EvKey (KChar 'd') [MCtrl]) = halt s
handleVtyEvent s (EvKey k mod) = continue s
handleVtyEvent s (EvResize k y) = continue s
handleVtyEvent s _ = continue s

handleAppEvent :: State -> TCP.Event -> EventM Resource (Next State)
handleAppEvent s e = continue (s {stateLog = (show e):(stateLog s)})
--handleAppEvent s (TCP.EventFail msg) =
--  continue (s {stateLog = ("Failed to parse TCP. message: " ++ msg):(stateLog s)})
--handleAppEvent s (TCP.EventLog msg) =
--  continue (s {stateLog = msg:(stateLog s)})
--handleAppEvent s (TCP.EventTerrain t) =
--  continue (s {stateTerrain = t})
--handleAppEvent s (TCP.EventEntityAdd e) =
--  continue (s {stateEntities = e:(stateEntities s)})
--handleAppEvent s (TCP.EventEntityRemove eid) =
--  continue (s {stateEntities = filter (\e -> E.entityId e /= eid) (stateEntities s)})
--handleAppEvent s (TCP.EventPlayerId eid) =
--  if isJust player then
--    continue (s {statePlayer = fromJust player})
--  else
--    handleAppEvent s (TCP.EventFail $ "Failed to find player entity with Id " ++ show eid)
--  where player = findEntity eid s
