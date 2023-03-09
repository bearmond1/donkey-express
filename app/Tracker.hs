module Tracker where

import Control.Monad.Trans.Maybe 

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Exception     (try)
import Data.Maybe            (catMaybes)
import Data.List as L hiding (unlines,sum)
import Data.Char             (chr, ord)
import Data.Text as Text
import Data.Text.Encoding
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
import Types


targetPeersNum = 25

-- 3 sec
respTimeout = 3000000


-- initial peers collecting. unresponding trackers left aside
-- gathering as much peers as possible in given time
collectPeers :: TFile -> [Word8] -> (Text,Text) -> IO (TFile,[TrackerResponse])
collectPeers tFile clientID (host,port) = do 
  httpTrackers <- getHTTPtrackers
  let list = PL.take 7 $ tFile.announce : (PL.concat tFile.announceList) ++ httpTrackers
  --print $ "collectPeers " ++ show list
  responses <- mapConcurrently (\url -> runMaybeT $ initTrackerReq tFile clientID url (host,port) ) list
  --print "end collecting peers"
  return (tFile,catMaybes responses)



initTrackerReq :: TFile -> [Word8] -> URL -> (Text,Text) -> MaybeT IO TrackerResponse
initTrackerReq tFile clientID url (host,port) = 
  trackerRequest clientID (host,port) tFile url (Just "started") 0 0
  
  
  
-- perform announce request on given single URL
trackerRequest :: [Word8] -> (Text,Text)-> TFile -> URL -> Maybe TrackerEvent -> Uploaded -> Downloaded -> MaybeT IO TrackerResponse
trackerRequest clientID (host,port) tFile url mbEvent upl downl= do
  print "trackerRequest"
  let totalLength = getTotalLength tFile

  -- later to implement UDP trackers support
  request' <- hoistMaybe (parseRequest ( Text.unpack url ) :: Maybe Request)
  let request = 
        setRequestResponseTimeout (responseTimeoutMicro respTimeout) $
        setRequestQueryString
         [ ("info_hash", Just tFile.infoHash)
         , ("peer_id", Just $ BS.pack clientID)
         , ("ip", Just $ encodeUtf8 host)
         , ("port", Just $ encodeUtf8 port) -- to place port into enviroment
         , ("uploaded", Just $ show upl) -- total number of bytes uploaded in base ten ASCII
         , ("downloaded", Just $ show downl) -- total number of bytes downloaded in base ten ASCII
         , ("left", Just $ show $ totalLength - downl) -- The number of bytes to download in base ten ASCII
         , ("event", fmap encodeUtf8 mbEvent) ]  -- omitted for regular updates         
         request'
  
  mbResponse <- liftIO $ catch (fmap Just $ Simple.httpLbs request) (\(e :: HttpException) -> return Nothing)
  trResponse <- hoistMaybe mbResponse
  bencode <- hoistMaybe $ bRead $ getResponseBody trResponse
  response <- hoistMaybe $ rightToMaybe $ runParser (responseParser url) bencode 
  --print response
  return response

getTotalLength :: TFile -> Integer
getTotalLength tFile = case tFile.info of
                         SingleFileInfo{length} -> length
                         MultipleFilesInfo{..} -> pieceLength * toInteger (PL.length files)


hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure
  
  
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




responseParser :: URL -> BParser TrackerResponse
responseParser url = do
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
      -- possibly there are hexed IPv6 and DNS names 
      host <- fmap fromInteger $ bint  $ dict "ip"
      port <- fmap fromInteger $ bint $ dict "port"
      return $ (peerID,SockAddrInet port host)

    binaryPeers = do
      bs <- bbytestring $ dict "peers"
      return $ decodeIPv4 $ LBS.toStrict bs

    decodeIPv4 :: BS.ByteString -> [(Text,SockAddr)]
    decodeIPv4 bs | BS.null bs = []
                  | mod (BS.length bs) 6 == 0 = 
                       let (ip, r1) = BS.splitAt 4 bs
                           (port', r2) = BS.splitAt 2 r1
                           (a:b:c:d:[]) = BS.unpack ip
                           host = tupleToHostAddress (a,b,c,d)
                           (e:f:[]) = BS.unpack port'
                           port = fromInteger $ 256 * (toInteger e) + (toInteger f) 
                       in ("",(SockAddrInet port host)) : decodeIPv4 r2
                  | otherwise = [] 


-- biggest response is the one with lognest interval
instance Ord TrackerResponse where
  compare tr1 tr2 = compare tr1.interval tr2.interval


-- process to keep data syncronized with trackers
followTrackers :: [Word8] -> (Text,Text) -> TVar NetState -> IO ()
followTrackers clientID (host,port) tVar = do
  tState <- atomically $ readTVar tVar
  --print tState
  -- take biggest tracker polling interval 
  let commonInterval = interval $ L.maximum tState.trResponses
  print $ "commonInterval is " ++ show commonInterval
  
  -- polling trackers
  newTrResponses' <- mapConcurrently 
    (\TrackerResponse{url} -> runMaybeT $ trackerRequest clientID (host,port) tState.tFile url Nothing tState.uploaded tState.downloaded) 
      tState.trResponses

  -- fetching new peers and write `em to the torrent state
  let newTrResponses = catMaybes newTrResponses' 
  let newState = tState{ trResponses = L.union newTrResponses tState.trResponses
                       , peers = newPeers tState ( L.concat $ L.map (\TrackerResponse{tPeers} -> tPeers) newTrResponses)
                       }
  atomically $ writeTVar tVar newState
  
  -- sleep and repeat
  threadDelay $ (fromInteger commonInterval) * 1000000
  followTrackers  clientID (host,port) tVar



-- add new peers to the list
newPeers :: NetState -> [(Text,SockAddr)] -> [Peer]
newPeers NetState{ peers } resps = 
  let func :: (Text,SockAddr) -> [Peer] -> [Peer]
      func (peerID,sockAddr) knownPeers = 
        if PL.any (comparation sockAddr) knownPeers
          then knownPeers
          else (NewPeer peerID sockAddr) : knownPeers
      
      comparation sockAddr ActivePeer{addr} = sockAddr == addr
      comparation sockAddr NewPeer{addr} = sockAddr == addr
      
  in L.foldr func peers resps