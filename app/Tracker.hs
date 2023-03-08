module Tracker where

import Control.Monad.Trans.Maybe 

import Control.Concurrent
import Control.Concurrent.Async
-- import Control.Concurrent.STM
-- import Control.Concurrent.STM.TVar

import Control.Exception     (try)
import Data.Maybe            (catMaybes)
import Data.List as L hiding (unlines,sum)
import Data.Char             (chr, ord)
import Data.Text as Text
import Protolude as PL

import Network.HTTP.Client as Client
import Network.HTTP.Simple as Simple
import Network.Socket

import Data.BEncode as BE
import Data.BEncode.Parser as BEP
--import Network.URI.Encode (encodeByteString)
import Data.ByteString as BS 
import Data.ByteString.Lazy as LBS 
import System.Random
import TorrentFile


targetPeersNum = 25

data TrackerResponse
  = TrackerResponse
  { interval :: Integer
  , minInterval :: Integer
  , trackerID :: Maybe Integer
  , complete :: Integer
  , incomplete :: Integer
  , tPeers :: [(Text,SockAddr)]
  }
  deriving (Eq,Show)
  
data PeerInfo
  = PeerInfo
  { peerID :: Text
  , ip :: Int
  , port :: Int
  }
  deriving (Eq,Show)

  
type URL = Text

-- 3 sec
respTimeout = 3000000


-- gathering as much peers as possible in given time
collectPeers :: TFile -> [Word8] -> IO (TFile,[TrackerResponse])
collectPeers tFile clientID = do 
  httpTrackers <- getHTTPtrackers
  let list = PL.take 7 $ tFile.announce : (PL.concat tFile.announceList) ++ httpTrackers
  --print $ "collectPeers " ++ show list
  responses <- mapConcurrently (\url -> runMaybeT $ trackerRequest tFile clientID url) list
  print "end collecting peers"
  return (tFile,catMaybes responses)
  
  
-- perform announce request on given single URL
trackerRequest :: TFile -> [Word8] -> URL -> MaybeT IO TrackerResponse 
trackerRequest tFile clientID url = do
  --liftIO $ print "tracker req"
  let totalLength = case tFile.info of
                       SingleFileInfo{length} -> length
                       MultipleFilesInfo{..} -> pieceLength * toInteger (PL.length files)

  -- later to implement UDP trackers support
  request' <- hoistMaybe (parseRequest ( Text.unpack url ) :: Maybe Request)
  let request = 
        setRequestResponseTimeout (responseTimeoutMicro respTimeout) $
        setRequestQueryString
         [ ("info_hash", Just tFile.infoHash)
         , ("peer_id", Just $ BS.pack clientID)
         , ("port", Just "6881") -- to place port into enviroment
         , ("event", Just "started")
         , ("uploaded", Just "0")
         , ("downloaded", Just "0")
         , ("left", Just "0") ] 
         request'
  
  mbResponse <- liftIO $ catch (fmap Just $ Simple.httpLbs request) (\(e :: HttpException) -> return Nothing)
  response <- hoistMaybe mbResponse
  --print response
  bencode <- hoistMaybe $ bRead $ getResponseBody response
  hoistMaybe $ rightToMaybe $ runParser responseParser bencode
  -- to check peer SockAddr here
    
  
  
-- public list of open trackers
getHTTPtrackers :: IO [URL]
getHTTPtrackers = do
  request <- parseRequest $ Text.unpack knownHTTPTrackersList
  response <- Simple.httpBS request
  let trackers' = case getResponseStatusCode response of
                 200 -> Text.lines $ decodeUtf8 $ getResponseBody response
                 _ -> []
  return $ PL.filter (/= "") trackers'
  
  
knownHTTPTrackersList :: Text
knownHTTPTrackersList = "https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_all_http.txt"  
  
knownHTTPSTrackersList :: Text
knownHTTPSTrackersList = "https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_all_https.txt"




responseParser :: BParser TrackerResponse
responseParser = do
  interval <- bint $ dict "interval"
  minInterval <- bint $ dict "min interval"
  trackerID <- BEP.optional $ bint $ dict "tracker id"
  complete <- bint $ dict "complete"
  incomplete <- bint $ dict "incomplete"
  downloaded <- bint $ dict "downloaded"
  
  tPeers <- (BEP.list "peers" dictPeers) <|> binaryPeers
  
  return TrackerResponse{..}
  
  where
    dictPeers = do
      peerID <- fmap Text.pack $ bstring $ dict "peer id"
      host <- fmap fromInteger $ bint  $ dict "ip" -- can be DNS name
      port <- fmap fromInteger $ bint $ dict "port"
      return $ (peerID,SockAddrInet port host)

    binaryPeers = do
      bs <- bbytestring $ dict "peers"
      return $ decodeIps4 $ LBS.toStrict bs
    
    decodeIps6 :: BS.ByteString -> [(Text,SockAddr)]
    decodeIps6 bs | BS.null bs = []
                  | BS.length bs >= 18 =
                        let (ip6, r1) = BS.splitAt 16 bs
                            (port, r2) = BS.splitAt 2 r1
                            i' = cW128 ip6
                            p' = fromIntegral $ cW16 port
                        in ("",SockAddrInet6 p' 0 i' 0) : decodeIps6 r2
                  | otherwise = [] 

    -- decodeIps4 :: BS.ByteString -> [(Text,SockAddr)]
    -- decodeIps4 bs | BS.null bs = []
                  -- | BS.length bs >= 6 = 
                       -- let (ip, r1) = BS.splitAt 4 bs
                           -- (port, r2) = BS.splitAt 2 r1
                           -- i' = cW32 ip
                           -- p' = fromIntegral $ cW16 port
                       -- in ("",(SockAddrInet p' i')) : decodeIps4 r2
                  -- | otherwise = [] 
                  
    decodeIps4 :: BS.ByteString -> [(Text,SockAddr)]
    decodeIps4 bs | BS.null bs = []
                  | mod (BS.length bs) 6 == 0 = 
                       let (ip, r1) = BS.splitAt 4 bs
                           (port', r2) = BS.splitAt 2 r1
                           (a:b:c:d:[]) = BS.unpack ip
                           host = tupleToHostAddress (a,b,c,d)
                           (e:f:[]) = BS.unpack port'
                           port = fromInteger $ 256 * (toInteger e) + (toInteger f) 
                       in ("",(SockAddrInet port host)) : decodeIps4 r2
                  | otherwise = [] 

    cW128 :: BS.ByteString -> (Word32, Word32, Word32, Word32)
    cW128 bs =
        let (q1, r1) = BS.splitAt 4 bs
            (q2, r2) = BS.splitAt 4 r1
            (q3, q4) = BS.splitAt 4 r2
        in (cW32 q1, cW32 q2, cW32 q3, cW32 q4)
    
    cW32 :: BS.ByteString -> Word32
    cW32 bs = fromIntegral . sum $ s
      where up = BS.unpack bs
            s  = [ fromIntegral b `shiftL` sa | (b, sa) <- PL.zip up [0,8,16,24]] :: [Word32]

    cW16 :: BS.ByteString -> Word16
    cW16 bs = fromIntegral . sum $ s
      where s = [ fromIntegral b `shiftL` sa | (b, sa) <- PL.zip (BS.unpack bs) [0,8]] :: [Word16]
    
    to6Bytes :: BS.ByteString -> [BS.ByteString]
    to6Bytes bs | bs == (BS.empty) = []
    to6Bytes list = [BS.take 6 list] ++ to6Bytes (BS.drop 6 list) 





hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure