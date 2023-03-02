module Main (main) where

import Protolude as PL
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import TorrentFile
import Tracker
import System.Directory
import Data.Text.Encoding
import Data.Text as Text 


type Torrents = [(TFile,[TrackerResponse])]


main :: IO ()
main = do
  files' <- listDirectory torrentsFolder
  setCurrentDirectory torrentsFolder
  files <- mapM makeAbsolute files'
  tResponses <- atomically $ newTVar []
  mapM (forkIO . (processTFile tResponses)) files  
  wait tResponses files
  where 
    wait tResponses files = do
      responses <- readTVarIO tResponses
      if PL.length responses < PL.length files
        then (threadDelay 1000000) >> (wait tResponses files)
        else print "end"


  

processTFile :: TVar Torrents -> FilePath -> IO ()
processTFile tvar path = do
  tFile <- parseTFile path
  tResps <- collectPeers tFile
  atomically $ modifyTVar tvar ((tFile,tResps) : )
  print tResps
  
  
torrentsFolder :: FilePath
torrentsFolder = "C:\\Users\\madOr\\Downloads\\torrents"