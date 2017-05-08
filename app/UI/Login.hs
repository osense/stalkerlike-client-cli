module UI.Login where
import Network.Connection
import Data.Aeson
import System.Exit (exitSuccess)
import Brick
import Brick.BChan
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty
import Control.Monad.IO.Class
import TCP.Util
import qualified Data.TCP as TCP


data Res = ResName | ResPass
  deriving (Eq, Ord, Show)

data State = State
  { stateCon :: Connection
  , stateEditName :: Editor String Res
  , stateEditPass :: Editor String Res
  , stateFocused :: Res
  , stateMotd :: String
  }


runLogin :: Connection -> BChan TCP.Event -> IO State
runLogin con chan = customMain
  (Graphics.Vty.mkVty Graphics.Vty.defaultConfig)
  (Just chan) app (initialState con)

app :: App State TCP.Event Res
app = App
  { appDraw = draw
  , appChooseCursor = const (Just . head)
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap Graphics.Vty.defAttr []
  }

initialState :: Connection -> State
initialState con = State
  { stateCon = con
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
        res <- liftIO $ attemptLogin (stateCon s) name pass
        either (\err -> continue s)
               (const (halt s))
               res
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
handleAppEvent s (TCP.EventLoginFail info) = continue (s {stateMotd = info})
handleAppEvent s _ = continue s

attemptLogin :: Connection -> String -> String -> IO (Either String ())
attemptLogin = undefined
--attemptLogin con user pass = do
--  putJSON (TCP.CommandLogin user pass)
--  _ <- connectionPut con (BS.pack "\n")
--  _ <- connectionWaitForInput con (-1)
--  reply <- connectionGetLine 1000 con
--  let res = maybe TCP.LoginEvenFail id $ decodeStrict reply
--  if res == TCP.LoginEventSuccess
--    then return (Right ())
--    else return (Left (BS.unpack reply))
