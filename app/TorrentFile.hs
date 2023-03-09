module TorrentFile where

import Protolude
import Data.Text as Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar
import Data.BEncode as BE
import Data.BEncode.Parser as BEP
import Data.ByteString as BS 
import Data.ByteString.Lazy as LBS 
import Data.Map
import System.Environment  

import Crypto.Hash as Hash
import Data.ByteArray as BA (unpack)



data TFile 
  = TFile
  { info :: TFilesInfo
  , infoHash :: BS.ByteString
  , announce :: Text
  , announceList :: [[Text]]
  , creationDate :: Maybe Day
  , comment :: Maybe Text
  , createdBy :: Maybe Text
  , encoding :: Maybe Text
  }
  deriving (Eq,Show)
  
  
data TFilesInfo
  = SingleFileInfo
  { pieceLength :: Integer
  , pieces :: BS.ByteString
  , private :: Maybe Integer
  , fileName :: BS.ByteString
  , length :: Integer}
  |
  MultipleFilesInfo
  { pieceLength :: Integer
  , pieces :: BS.ByteString
  , private :: Maybe Integer
  , directoryName :: FilePath
  , files :: [FileInfo] -- "dir1/dir2/file.ext" ==  ["dir1", "dir2", "file.ext"]
  }
  deriving (Eq,Show)
  
  
data FileInfo
  = FileInfo
  { length :: Integer
  , path :: [Text]
  }
  deriving (Eq,Show)





parseTFile :: FilePath -> IO TFile
parseTFile path = do
  bs <- BS.readFile path

  benc <- case bRead $ BS.fromStrict bs of
            Nothing -> undefined
            Just bc -> return bc
  case runParser torrentInfoReader benc of
    Right tf -> return tf
    _ -> undefined
	
	

torrentInfoReader :: BParser TFile
torrentInfoReader = do
  announce <- fmap Text.pack $ bstring $ dict "announce" 
  info' <- dict "info" 
  info <- case runParser tFilesReader info' of
            Right info -> return info
            Left e -> mzero

  announceList' <- BEP.optional $ (dict "announce-list") >>= getBList >>= (mapM getBList) >>= 
                   (mapM $ mapM  getStr)

  let announceList = case announceList' of
                       Just list -> list
                       Nothing -> []

  creationDate <- BEP.optional $ fmap ModifiedJulianDay $ bint $ dict "creation date" 
  comment <- BEP.optional $ fmap Text.pack $ bstring $ dict "comment"
  createdBy <- BEP.optional $ fmap Text.pack $ bstring $ dict "created by"
  encoding <- BEP.optional $ fmap Text.pack $ bstring $ dict "encoding"
  let infoHash = getInfoHash info'
  return TFile {..}


getInfoHash :: BEncode -> BS.ByteString
getInfoHash bc = BS.pack $ BA.unpack (Hash.hash (BS.toStrict $ bPack bc) :: Digest SHA1)


getBList :: BEncode -> BParser [BEncode]
getBList bc = 
  case bc of 
    BList list -> return list
    _ -> mzero



getStr :: BEncode -> BParser Text
getStr bc = 
  case bc of
    BString bs -> return $ decodeUtf8 $ LBS.toStrict bs
    _ -> mzero



tFilesReader :: BParser TFilesInfo
tFilesReader = singleF <|> multiF
  where 
    singleF = do
      length <- bint $ dict "length"
      fileName <- fmap BS.toStrict $ bbytestring $ dict "name"
      pieceLength <- bint $ dict "piece length"
      pieces <- fmap BS.toStrict $ bbytestring $ dict "pieces"
      private <- BEP.optional $ bint $ dict "private"
      return SingleFileInfo{..}
    multiF = do
      pieceLength <- bint $ dict "piece length"
      pieces <- fmap BS.toStrict $ bbytestring $ dict "pieces"
      private <- BEP.optional $ bint $ dict "private"
      directoryName <- bstring $ dict "name"
      files <- BEP.list "files" tFilesInfo
      return MultipleFilesInfo{..}


tFilesInfo :: BParser FileInfo
tFilesInfo = do
  length <- bint $ dict "length"
  path <- BEP.list "path" $ fmap Text.pack $ bstring token 
  return FileInfo{..}