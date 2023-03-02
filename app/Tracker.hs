module Tracker where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

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
  , peers :: [SockAddr]
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


collectPeers :: TFile -> IO [TrackerResponse]
collectPeers tFile = do 
  httpTrackers <- getHTTPtrackers
  let list = tFile.announce : (PL.concat tFile.announceList) ++ httpTrackers
  tVar <- atomically $ newTVar []
  mapM (forkIO . (trackerRequest' tFile tVar)) list
  threadDelay (respTimeout*2)
  readTVarIO tVar
  
  
-- perform announce request on givel single URL
trackerRequest' :: TFile -> TVar [TrackerResponse] -> URL -> IO ()
trackerRequest' tFile tVar url = do

  (peer_id :: [Word8]) <- initStdGen >>= (\x -> return $ PL.take 20 $ randoms x )
  
  let totalLength = case tFile.info of
                       SingleFileInfo{length} -> length
                       MultipleFilesInfo{..} -> pieceLength * toInteger (PL.length files)

  request' <- parseRequest $ Text.unpack url
  let request = 
        setRequestResponseTimeout (responseTimeoutMicro respTimeout) $
        setRequestQueryString
         [ ("info_hash", Just tFile.infoHash)
         , ("peer_id", Just $ BS.pack peer_id)
         , ("port", Just "6881") -- to place port into enviroment
         , ("event", Just "started")
         , ("uploaded", Just "0")
         , ("downloaded", Just "0")
         , ("left", Just "0") ] 
         request'

  eResponse <- try $  Simple.httpLbs request
  
  case eResponse of
    Right response ->
      case bRead $ getResponseBody response of
       Nothing -> return ()
       Just bc -> 
         case runParser responseParser bc of
           Right tResponse -> do
             atomically $ modifyTVar tVar (tResponse : )
             return ()

           _ -> return ()
    Left e -> (print (e :: HttpException)) >> return ()

    
  
  
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
  
  peers <- (BEP.list "peers" dictPeers) <|> binaryPeers
  
  return TrackerResponse{..}
  
  where
    dictPeers = do
      peerID <- fmap Text.pack $ bstring $ dict "peer id"
      ip <- fmap fromInteger $ bint $ dict "ip"
      port <- fmap fromInteger $ bint $ dict "port"
      return $ SockAddrInet 0 minBound

    binaryPeers = do
      bs <- bbytestring $ dict "peers"
      let peersInfo = \(ip,port) -> SockAddrInet port ip 
      return $ decodeIps4 $ LBS.toStrict bs
      

    decodeIps4 :: BS.ByteString -> [SockAddr]
    decodeIps4 bs | BS.null bs = []
                  | BS.length bs >= 6 = 
                       let (ip, r1) = BS.splitAt 4 bs
                           (port, r2) = BS.splitAt 2 r1
                           i' = cW32 ip
                           p' = fromIntegral $ cW16 port
                       in (SockAddrInet p' i') : decodeIps4 r2
                  | otherwise = [] 

    
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