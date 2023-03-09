module Main (main) where

import Protolude as PL
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import TorrentFile
import Tracker
import Types
import PeerWireProtocol
import System.Directory
import System.Random
import Data.Word
import Data.Text.Encoding
import Data.Text as Text 

import Network.BSD
import Network.Socket                            
import System.IO hiding (print)




main :: IO ()
main = do
  let (host,port) = ("158.181.223.125","6881")
  clientID <- getClientID -- to put into config
  files' <- listDirectory torrentsFolder
  setCurrentDirectory torrentsFolder
  files <- mapM makeAbsolute files'
  mapConcurrently (runTorrent clientID (host,port))files
  return ()
  
  
  -- torrents <- mapConcurrently (\path -> parseTFile path >>= (\tFile -> collectPeers tFile clientID (host,port))) files 
  -- print "gathered torrents"
  -- -- rigth now state isnt kept between sessions, so iHave == []
  -- -- create initial torrents states and put them into TVars
  -- let initStates = PL.map (\(tFile,resps) -> NetState tFile resps [] [] 0 0 (getTotalLength tFile)) torrents
  -- tVars <- mapM ( atomically . newTVar ) initStates
  
  -- -- start processes that do the talking to the tracker
  -- print "followTrackers"
  -- mapConcurrently (followTrackers clientID (host,port)) tVars
  -- return ()
  -- processes which talks to peers
  -- TCP server
  --initializeTorrents torrents ("158.181.223.125","6881") clientID


runTorrent :: [Word8] -> (Text,Text) -> FilePath -> IO ()
runTorrent clientID (host,port) path = do
  tFile <- parseTFile path
  torrent <- collectPeers tFile clientID (host,port)
  
  -- rigth now we dont keep looking for trackers
  when ((snd torrent) /= []) $ do
    let initState = NetState tFile (snd torrent) [] [] 0 0 (getTotalLength tFile)
    tVar <- atomically $ newTVar initState
    followTrackers clientID (host,port) tVar
  
  
  
torrentsFolder :: FilePath
torrentsFolder = "C:\\Users\\madOr\\Downloads\\torrents"


-- eventually its to be put into config 
getClientID :: IO [Word8]
getClientID = initStdGen >>= (\x -> return $ PL.take 20 $ randoms x )