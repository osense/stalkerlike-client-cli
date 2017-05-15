module Main where
import Control.Concurrent
import Network.Connection (Connection)
import Brick
import Brick.BChan
import Graphics.Vty

import UI.Login (runLogin)
import UI.App (runApp)
import TCP.Client (connect)


main :: IO ()
main = do
  (downChan, upChan) <- connect "localhost" 31415
  runLogin downChan upChan
  runApp downChan upChan
  return ()
