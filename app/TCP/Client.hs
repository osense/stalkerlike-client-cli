module TCP.Client where
import Control.Concurrent
import Control.Concurrent.Async (concurrently)
import Control.Monad (void)
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Network
import Data.Conduit.TMChan (sourceTMChan)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecodeStrict)
import Data.Aeson.Parser (json)
import Control.Concurrent.STM.TMChan (TMChan, newTMChanIO)
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.TCP as TCP


connect :: String -> Int -> IO (BChan TCP.Event, TMChan TCP.Command)
connect host port = do
  downChan <- newBChan 10
  upChan <- newTMChanIO
  forkFinally (run host port downChan upChan) (\(Left e) -> writeBChan downChan (TCP.EventFail (show e)))
  return (downChan, upChan)

run :: String -> Int -> BChan TCP.Event -> TMChan TCP.Command -> IO ()
run host port downChan upChan = runTCPClient (clientSettings port (BS.pack host)) $ \server ->
  void $ concurrently
    (runConduit $ sourceTMChan upChan .| C.map (\x -> encodeStrict x `BS.append` (BS.pack "\n")) .| appSink server)
    (runConduit $ appSource server .| C.map (handleError . eitherDecodeStrict) .| C.mapM_ (writeBChan downChan))
  where encodeStrict = toStrict . encode
        toStrict = BS.pack . LBS.unpack
        handleError (Left e) = TCP.EventFail e
        handleError (Right x) = x
