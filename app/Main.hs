module Main where
import Control.Concurrent
import Network.Connection (Connection)
import Brick
import Brick.BChan
import Graphics.Vty

import UI.Login (runLogin)
import UI.App (runApp)
import TCP.Client (connect, startTCP)


main :: IO ()
main = do
  tcpChan <- newBChan 10
  tcpCon <- connect "localhost" 31415
  tcpThread <- startTCP tcpCon tcpChan
  runLogin tcpCon tcpChan
  runApp tcpCon tcpChan
  killThread tcpThread
