module PeerWireProtocol where

import Protolude as PL
import TorrentFile
import Tracker
import Types
import System.IO.Error (IOError)

--import Network.Run.TCP
import Network.Transport.Internal -- ?
import Network.Socket as S
import Network.Socket.ByteString as SBS
import qualified Network.Simple.TCP as TCP

import Data.ByteString as BS hiding (map)
import qualified Data.ByteString.Lazy as LBS 
import Data.Text.Encoding
--import Data.Bits
import Data.Word
import Data.Text as Text hiding (map) 
import Data.List as L hiding (map)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception as E
import Data.Attoparsec.ByteString as P







-- send handshake to all known peers and return state of p2p network regards to torrent
-- initializeTorrents :: Torrents -> (Text,Text) -> [Word8] -> IO ()
-- initializeTorrents torrents (host,port) clientID = do
  -- let states = map mkStates torrents
  -- tVars <- mapM ( atomically . newTVar ) states
  
  -- serverThread <- forkIO $ TCP.serve TCP.HostAny (Text.unpack port) (tcpServer tVars)
  
  -- -- [TrackerResponse] -> [SocketAddr]
  -- let socketList resps = L.nub $ PL.concat $ map (\TrackerResponse {tPeers} -> map snd tPeers) resps
  -- -- :: [(TFile,[SocketAddr])]
      -- fileAndSockets = map (\(file,list) -> (file,socketList list)) torrents
  -- -- (TFile,[Sockets]) -> [(TFile,SocketAddr)]
      -- fileAndSocket (tFile,sockets) = L.zip (repeat tFile) sockets 
      -- peersList = L.nub $ L.concat $ map fileAndSocket fileAndSockets
  
  -- print $ "torrents length " ++ show (L.length torrents )
  -- print $ "peers length " ++ show (L.length peersList )
  
  -- forever $ do
    -- result <- mapConcurrently (\peer -> catch (handshakePeer clientID peer) (\(e :: IOError) -> print e >> ( return () ) ) ) peersList
    -- threadDelay $ 60*1000000
    
  -- return ()
  
  -- where
    -- mkStates (tFile,resps) = 
      -- NetState
      -- { tFile = tFile
      -- , trResponses = resps
      -- , peers = L.nub . PL.concat $ map toPeer resps
      -- , iHave = []
      -- }
      
    -- toPeer TrackerResponse{ tPeers } = map respToPees tPeers
    -- respToPees (peerID,socketAddr) = 
      -- Peer
      -- { peerID = peerID
      -- , addr = socketAddr
      -- , isConnected = False
      -- , amChoking = True
      -- , amInterested = False
      -- , peerChoking = True
      -- , peerInterested = False
      -- , hasPieces = []
      -- }



-- mark peer as connected if got handshake in return
tcpServer :: [TVar NetState] -> (Socket,SockAddr) -> IO ()
tcpServer states (connSocket,sockAddr) = do
  bs <- SBS.recv connSocket 1024
  print $ BS.length bs
  addrInfo <- getPeerName connSocket
  putStrLn $ "TCP connection established from " ++ show addrInfo
  msg <- TCP.recv connSocket 1024 -- handshake length is 68 bytes
  print $ "recieved message length is " ++ show (fmap BS.length msg)
  --print msg
  -- case msg >>= checkHandshake of
    -- Just hs -> atomically $ modifyTVar tVar $ modifSt hs
    -- Nothing -> return ()

  -- where
    -- modifSt :: Handshake -> NetState -> NetState
    -- modifSt hs st = case fetch hs st of
       -- Just a -> 
         -- let newList = a{ isConnected = True} : ( L.delete a st.peers)
         -- in st{ peers = newList } :: NetState
       -- Nothing -> st
    -- fetch hs st = L.find (\peer -> peer.peerID == hs.peerID) st.peers



-- handshake: <pstrlen><pstr><reserved><info_hash><peer_id>
handshakePeer :: [Word8] -> (TFile,SockAddr) -> IO ()
handshakePeer clientID (tFile,socketAddr)= do

  socket <- S.openSocket $ S.AddrInfo [S.AI_ALL] AF_INET Stream S.defaultProtocol socketAddr Nothing
  S.connect socket socketAddr
  print $ "connected to " ++ show socketAddr

  let pstrlen = BS.singleton 19
      pstr = encodeUtf8 "BitTorrent protocol"
      reserved = BS.pack $ PL.replicate 8 (0 :: Word8)
      peer_id = BS.pack clientID
      handshakeMSG = BS.concat [pstrlen,pstr,reserved,tFile.infoHash,peer_id]

  lngth <- SBS.send socket handshakeMSG
  
  bs <- SBS.recv socket 1024
  print $ BS.length bs



data Handshake 
  = Handshake
  { pstrlen :: [Word8]     -- 1 Byte
  , pstr :: Text            
  , reserved :: [Word8]    -- 8 Bytes
  , infoHash :: ByteString
  , peerID :: Text
  }
  deriving (Eq,Show)


checkHandshake :: BS.ByteString -> Maybe Handshake
checkHandshake bs = Nothing
  -- let parser = 
        -- Handshake <$>
        -- (P.take 1 <*> word8 19) <*> 
        -- string "BitTorrent protocol" <*> 
        -- P.take 8 <*> 
        -- P.take 20 <*>
        -- P.take 20
  -- in
    -- case parse parser bs of
      -- Done _ hs -> Just hs
      -- _ -> Nothing