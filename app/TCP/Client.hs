module TCP.Client where
import Control.Concurrent
import Network
import GHC.IO.Handle
import Brick.BChan
import Data.TCPEvent
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe (fromMaybe)


startTCP :: BChan TCPEvent -> IO (Handle, ThreadId)
startTCP chan = do
  handle <- connect
  thread <- forkIO (loop handle chan)
  return (handle, thread)

connect :: IO Handle
connect = connectTo "localhost" (PortNumber 31415)

loop :: Handle -> BChan TCPEvent -> IO ()
loop h chan = do
  input <- hGetLine h
  let msg = fromMaybe (TCPEventFail input) $ decode (pack input)
  writeBChan chan msg
  loop h chan
