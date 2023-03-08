module Main (main) where

import Protolude as PL
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import TorrentFile
import Tracker
import PeerWireProtocol
import System.Directory
import System.Random
import Data.Word
import Data.Text.Encoding
import Data.Text as Text 

import Network.BSD
import Network.Socket                            
import System.IO 

main :: IO ()
main = do    
  -- proto <- getProtocolNumber "tcp"                                    
  -- sock <- socket AF_INET Stream proto               
  -- bind sock (SockAddrInet 6881 0)
  -- listen sock 2                                  
  -- System.IO.putStrLn "Listening on port 6881..."           

  -- loopForever sock       
  
  --runTCPServer (Just "77.235.5.98") "6881" (tcpServer []) 
  -- hhh
  clientID <- getClientID
  files' <- listDirectory torrentsFolder
  setCurrentDirectory torrentsFolder
  files <- mapM makeAbsolute files'
  torrents <- mapConcurrently (\path -> parseTFile path >>= (\tFile -> collectPeers tFile clientID)) files  
  initializeTorrents torrents ("77.235.5.98","6881") clientID



-- loopForever :: Socket -> IO ()                   
-- loopForever sock = do                            
  -- (conn, _) <- accept sock                       
  -- handleSock <- socketToHandle conn ReadWriteMode

  -- line <- hGetLine handleSock                    
  -- System.IO.putStrLn $ "Request received: " ++ line        

  -- System.IO.hPutStrLn handleSock $ "Hey, client!"          
  -- hClose handleSock                              
  -- loopForever sock      
  
  
  
torrentsFolder :: FilePath
torrentsFolder = "C:\\Users\\madOr\\Downloads\\torrents"


-- eventually its to be put into config 
getClientID :: IO [Word8]
getClientID = initStdGen >>= (\x -> return $ PL.take 20 $ randoms x )