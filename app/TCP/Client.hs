module TCP.Client where
import Control.Concurrent
import Data.Default
import Control.Concurrent.Async (concurrently)
import Control.Monad (void)
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Network
import Data.Conduit.TMChan (sourceTMChan)
import Data.Conduit.Attoparsec (sinkParserEither, conduitParserEither)
import Data.Aeson (FromJSON, ToJSON, encode, decodeStrict)
import Data.Aeson.Parser (json)
import Control.Concurrent.STM.TMChan (TMChan, newTMChanIO)
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS


connect :: (FromJSON a, ToJSON b) => String -> Int -> IO (BChan a, TMChan b)
connect host port = do
  downChan <- newBChan 10
  upChan <- newTMChanIO
  forkIO $ runTCPClient (clientSettings port (BS.pack host)) $ \server ->
    void $ concurrently
      (runConduit $ sourceTMChan upChan .| C.map encode' .| appSink server)
      (runConduit $ appSource server .| C.map (handleError . decodeStrict) .| C.mapM_ (writeBChan downChan))
  return (downChan, upChan)
  where encode' = toStrict . encode
        toStrict = BS.pack . LBS.unpack
        handleError Nothing = undefined
        handleError (Just x) = x
