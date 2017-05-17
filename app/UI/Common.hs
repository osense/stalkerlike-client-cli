module UI.Common where
import Control.Monad.IO.Class (liftIO)
import GHC.Conc.Sync (atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM.TMChan (TMChan, writeTMChan)
import Brick
import qualified Data.TCP as TCP


send :: TMChan TCP.Command -> TCP.Command -> EventM s ()
send chan msg = liftIO . atomically $ writeTMChan chan msg
