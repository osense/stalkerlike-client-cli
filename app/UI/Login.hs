module UI.Login where
import Control.Concurrent.STM.TMChan (TMChan, writeTMChan)
import GHC.Conc.Sync (atomically)
import Data.Aeson
import System.Exit (exitSuccess)
import Brick
import Brick.BChan
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty
import Control.Monad.IO.Class (liftIO)
import qualified Data.TCP as TCP


data Res = ResName | ResPass
  deriving (Eq, Ord, Show)

data State = State
  { stateChan :: TMChan TCP.Command
  , stateEditName :: Editor String Res
  , stateEditPass :: Editor String Res
  , stateFocused :: Res
  , stateMotd :: String
  }


runLogin :: BChan TCP.Event -> TMChan TCP.Command -> IO State
runLogin downChan upChan = customMain
  (Graphics.Vty.mkVty Graphics.Vty.defaultConfig)
  (Just downChan) app (initialState upChan)

app :: App State TCP.Event Res
app = App
  { appDraw = draw
  , appChooseCursor = const (Just . head)
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap Graphics.Vty.defAttr []
  }

initialState :: TMChan TCP.Command -> State
initialState upChan = State
  { stateChan = upChan
  , stateEditName = editor ResName (str . head) (Just 1) ""
  , stateEditPass = editor ResPass (str . head) (Just 1) ""
  , stateFocused = ResName
  , stateMotd = " "
  }

draw :: State -> [Widget Res]
draw s = [withBorderStyle unicodeRounded $ motd <=> login]
  where motd = center $ str (stateMotd s)
        login = center $ borderWithLabel (str "Login") $ hLimit 40 $ (name <=> pass)
        name = (str "Name: ") <+> renderEditor (stateFocused s == ResName) (stateEditName s)
        pass = (str "Pass: ") <+> renderEditor (stateFocused s == ResPass) (stateEditPass s)
        padAll = padLeft Max . padRight Max . padTop Max . padBottom Max

handleEvent :: State -> BrickEvent Res TCP.Event -> EventM Res (Next State)
handleEvent s (VtyEvent e) = handleVtyEvent s e
handleEvent s (AppEvent e) = handleAppEvent s e

handleVtyEvent :: State -> Event -> EventM Res (Next State)
handleVtyEvent s (EvKey (KChar 'd') [MCtrl]) = liftIO exitSuccess
handleVtyEvent s (EvKey KUp mod) = continue (s {stateFocused = ResName})
handleVtyEvent s (EvKey KDown mod) = continue (s {stateFocused = ResPass})
--handleEvent s (VtyEvent (EvKey KTab mod)) =
--  continue (s {stateFocused = if (stateFocused s) == ResName then ResPass else ResName})
handleVtyEvent s (EvKey KEnter mod) =
  let name = head . getEditContents $ stateEditName s
      pass = head . getEditContents $ stateEditPass s
  in
    if name /= "" && pass /= ""
      then do
        liftIO . atomically $ writeTMChan (stateChan s) (TCP.CommandLogin name pass)
        continue s
      else continue s
handleVtyEvent s e = do
  let foc = if (stateFocused s) == ResName then (stateEditName s) else (stateEditPass s)
  new <- handleEditorEvent e foc
  if (stateFocused s) == ResName
    then continue (s {stateEditName = new})
    else continue (s {stateEditPass = new})

handleAppEvent :: State -> TCP.Event -> EventM Res (Next State)
handleAppEvent s (TCP.EventLoginMOTD motd) = continue (s {stateMotd = motd})
handleAppEvent s TCP.EventLoginSuccess = halt s
handleAppEvent s (TCP.EventFail info) = continue (s {stateMotd = (stateMotd s) ++ "\nError: " ++ info})
handleAppEvent s _ = continue s
