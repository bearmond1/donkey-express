module Types where

import Protolude
import TorrentFile
import Network.Socket as S



data NetState
  = NetState
  { tFile :: TFile
  , trResponses :: [TrackerResponse]
  , peers :: [Peer]
  , iHave :: [Int] -- which pieces do we have
  , uploaded :: Integer
  , downloaded :: Integer
  , left :: Integer 
  }
  deriving (Eq,Show)

data Peer 
  = ActivePeer
  { peerID :: Text
  , addr :: SockAddr
  , amChoking :: Bool
  , amInterested :: Bool
  , peerChoking :: Bool
  , peerInterested :: Bool
  , hasPieces :: [Int]
  }
  | NewPeer
  { peerID :: Text
  , addr :: SockAddr
  }
  deriving (Eq,Show)

type Torrents = [(TFile,[TrackerResponse])]




data TrackerResponse
  = TrackerResponse
  { url :: Text
  , interval :: Integer
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
type TrackerEvent = Text
type Uploaded = Integer
type Downloaded = Integer