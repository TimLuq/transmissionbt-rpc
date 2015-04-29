{-# LANGUAGE OverloadedStrings, GADTs, RankNTypes, ImpredicativeTypes, FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, DeriveDataTypeable #-}
module Network.BitTorrent.Transmission.Internal where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Dynamic
import Data.Typeable
import Data.Maybe (catMaybes)
import Prelude as P
import Data.List as P (intersperse)
import Data.Text as T
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.ByteString.Lazy as BS
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<|>), (<*>))
import Control.Monad.State (get)
import Control.Concurrent.MVar
import qualified Network.HTTP as HTTP
import qualified Network.Browser as HTTP
import Network.URI (URI)

--import Data.Foldable
import Data.Monoid
--import Data.Aeson.Types as JSON

import Data.HashMap.Strict as M
--import Data.Word (Word32, Word8)
import Data.Int (Int64)

--import Data.Attoparsec.ByteString (parseOnly)
--import Data.Attoparsec.Number as APN (Number(..))
--import Data.Scientific as Sci

type TorrentPriority = Int
type BandwidthPriority = TorrentPriority
type TorrentFilePriority = TorrentPriority
type TorrentHash = Text
type Tag = Int
type TorrentList = Maybe [TorrentHash]
type TagGenerator = (Text -> Value -> Tag)
type SpeedLimit = Int
type FileIndices = [Int]
type PeerLimit   = Int
type QueuePosition = Int
type TrackerAnnounce = Text
type TrackerId = Int
-- | Idle limit in minutes
type SeedIdleLimit = Int
-- | Ratio of upload:download. Between 2.0 and 5.0 recommended.
type SeedRatioLimit = Double
-- | A collection och key-value pairs for use as cookies for torrents
type Cookies = [(Text, Text)]

type SeedIdleMode = SettingsMode
type SeedRatioMode = SettingsMode

type M = M.HashMap

type TorrentFileId = Int
-- | A representation of a file. Use 'fileLength', 'fileName', and 'fileCompleted'.
newtype TorrentFile      = TorrentFileData  (Int64, Int64, Text) deriving (Eq, Show, Typeable)
-- | Another representation of a file. Use 'fileWanted', 'filePriority', and 'fileCompleted'.
newtype TorrentFileStats = TorrentFileStts  (Int64, Bool , Int ) deriving (Eq, Show, Typeable)
-- | A collection of 'TorrentFileId's.
newtype TorrentFileIds   = TorrentFileIds   (Maybe [TorrentFileId])  deriving (Eq, Show, Typeable)

-- | Simple information about a tracker.
newtype TorrentTracker   = Tracker   (Text, Int, Text, Int) deriving (Eq, Show, Typeable)
-- | In-depth information about a tracker. For basic use 'TorrentTracker' is the better option.
data TorrentTrackerStats = TrackerStats
  { trackerStatsAnnounce :: Text, announceState :: Int, trackerDownloadCount :: Int
  , hasAnnounced :: Bool, hasScraped :: Bool, trackerHost :: Text, trackerStatsId :: TrackerId
  , trackerIsBackup :: Bool, lastAnnouncePeerCount :: Int, lastAnnounceResult :: Text
  , lastAnnounceStartTime :: TorrentDate, lastAnnounceSucceeded :: Bool, lastAnnounceTime :: Int -- TODO: Should this be TorrentDate?
  , lastAnnounceTimedOut :: Bool, lastScrapeResult :: Text, lastScrapeStartTime :: TorrentDate
  , lastScrapeSucceeded :: Bool, lastScrapeTime :: Int -- TODO: Should this be TorrentDate?
  , lastScrapeTimedOut :: Bool, trackerLeecherCount :: Int, nextAnnounceTime :: TorrentDate, nextScrapeTime :: TorrentDate
  , trackerStatsScrape :: Text, scrapeState :: Int, trackerSeederCount :: Int, trackerStatsTier :: Int
  }
  deriving (Eq, Show, Typeable)

class TorrentTrackerData tracker where
  announce    :: tracker -> Text
  trackerId   :: tracker -> TrackerId
  scrape      :: tracker -> Text
  trackerTier :: tracker -> Int

instance TorrentTrackerData TorrentTracker where
  announce    (Tracker (a, _, _, _)) = a
  trackerId   (Tracker (_, a, _, _)) = a
  scrape      (Tracker (_, _, a, _)) = a
  trackerTier (Tracker (_, _, _, a)) = a
  
instance TorrentTrackerData TorrentTrackerStats where
  announce    = trackerStatsAnnounce
  trackerId   = trackerStatsId
  scrape      = trackerStatsScrape
  trackerTier = trackerStatsTier
  
instance FromJSON TorrentTracker where
  parseJSON (Object v) =
    (\a b c d -> Tracker (a, b, c, d)) <$>
    v .: "announce" <*> v .: "id" <*> v .: "scrape" <*> v .: "tier"
  parseJSON _ = mzero
instance ToJSON TorrentTracker where
  toJSON (Tracker (a, b, c, d)) =
    object ["announce" .= a, "id" .= b, "scrape" .= c, "tier" .= d]

instance FromJSON TorrentTrackerStats where
  parseJSON (Object v) =
    TrackerStats <$>
    v .: "announce" <*> v .: "announceState" <*> v .: "downloadCount" <*> v .: "hasAnnounced" <*> v .: "hasScraped" <*> v .: "host" <*> v .: "id" <*>
    v .: "isBackup" <*> v .: "lastAnnouncePeerCount" <*> v .: "lastAnnounceResult" <*> v .: "lastAnnounceStartTime" <*> v .: "lastAnnounceSucceeded" <*>
    v .: "lastAnnounceTime" <*> v .: "lastAnnounceTimedOut" <*> v .: "lastScrapeResult" <*> v .: "lastScrapeStartTime" <*> v .: "lastScrapeSucceeded" <*>
    v .: "lastScrapeTime" <*> v .: "lastScrapeTimedOut" <*> v .: "leecherCount" <*> v .: "nextAnnounceTime" <*> v .: "nextScrapeTime" <*> v .: "scrape" <*>
    v .: "scrapeState" <*> v .: "seederCount" <*> v .: "tier"
  parseJSON _ = mzero
instance ToJSON TorrentTrackerStats where
  toJSON t =
    object [ "announce" .= trackerStatsAnnounce t, "announceState" .= announceState t, "downloadCount" .= trackerDownloadCount t
           , "hasAnnounced" .= hasAnnounced t ,"hasScraped" .= hasScraped t, "host" .= trackerHost t, "id" .= trackerStatsId t
           , "isBackup" .= trackerIsBackup t, "lastAnnouncePeerCount" .= lastAnnouncePeerCount t, "lastAnnounceResult" .= lastAnnounceResult t
           , "lastAnnounceStartTime" .= lastAnnounceStartTime t, "lastAnnounceSucceeded" .= lastAnnounceSucceeded t
           , "lastAnnounceTime" .= lastAnnounceTime t, "lastAnnounceTimedOut" .= lastAnnounceTimedOut t, "lastScrapeResult" .= lastScrapeResult t
           , "lastScrapeStartTime" .= lastScrapeStartTime t, "lastScrapeSucceeded" .= lastScrapeSucceeded t, "lastScrapeTime" .= lastScrapeTime t
           , "lastScrapeTimedOut" .= lastScrapeTimedOut t, "leecherCount" .= trackerLeecherCount t, "nextAnnounceTime" .= nextAnnounceTime t
           , "nextScrapeTime" .= nextScrapeTime t, "scrape" .= trackerStatsScrape t, "scrapeState" .= scrapeState t
           , "seederCount" .= trackerSeederCount t, "tier" .= trackerStatsTier t
           ]

instance FromJSON TorrentFileIds where
  parseJSON x = (TorrentFileIds . Just) <$> parseJSON x
instance ToJSON TorrentFileIds where
  toJSON (TorrentFileIds x) = case x of
    Nothing -> JSON.Null
    Just x' -> toJSON x'

-- | A collection of 'TorrentFile's.
newtype TorrentStatsList = TorrentStatsList [TorrentFileStats]  deriving (Eq, Show, Typeable)

instance FromJSON TorrentStatsList where
  parseJSON x = TorrentStatsList <$> parseJSON x
instance ToJSON TorrentStatsList where
  toJSON (TorrentStatsList x) = toJSON x

class TorrentListable t a | t->a where
  asList :: t -> [a]

instance TorrentListable TorrentStatsList TorrentFileStats where
  asList (TorrentStatsList x) = x
instance TorrentListable TorrentFiles TorrentFile where
  asList (TorrentFileCol x) = x
  
-- | A collection of 'TorrentFile's.
newtype TorrentFiles     = TorrentFileCol   [TorrentFile]        deriving (Eq, Show, Typeable)

instance FromJSON TorrentFiles where
  parseJSON x = TorrentFileCol <$> parseJSON x
instance ToJSON TorrentFiles where
  toJSON (TorrentFileCol x) = toJSON x

instance FromJSON TorrentFile where
  parseJSON (JSON.Object v) = (\x y z -> TorrentFileData (x, y, z)) <$> v .: "bytesCompleted" <*> v .: "length" <*> v .: "name"
  parseJSON _ = mzero
instance ToJSON TorrentFile where
  toJSON (TorrentFileData (x, y, z)) = object ["bytesCompleted" .= x, "length" .= y, "name" .= z]

instance FromJSON TorrentFileStats where
  parseJSON (JSON.Object v) = (\x y z -> TorrentFileStts (x, y, z)) <$> v .: "bytesCompleted" <*> v .: "wanted" <*> v .: "priority"
  parseJSON _ = mzero
instance ToJSON TorrentFileStats where
  toJSON (TorrentFileStts (x, y, z)) = object ["bytesCompleted" .= x, "wanted" .= y, "priority" .= z]

-- | A list of 'TorrentFileId's representing all files.
allTorrentFileIds :: TorrentFileIds
allTorrentFileIds = TorrentFileIds $ Nothing

-- | The length (in bytes) of a file.
fileLength :: TorrentFile -> Int64
fileLength (TorrentFileData (_, x, _)) = x
-- | The name of a file.
fileName :: TorrentFile -> Text
fileName (TorrentFileData (_, _, x)) = x

-- | Whether or not this file should be downloaded.
fileWanted :: TorrentFileStats -> Bool
fileWanted (TorrentFileStts (_, x, _)) = x
-- | The download priority of a file.
filePriority :: TorrentFileStats -> Int
filePriority (TorrentFileStts (_, _, x)) = x

-- | General typeclass for a representation for a file where a number of bytes of completion is appropriate.
class FileCompleted a where
  -- | How many bytes of the file has been downloaded.
  -- When used with 'TorrentFile' the result may be compared with 'fileLength' to see if file is fully completed.
  fileCompleted :: a -> Int64

instance FileCompleted TorrentFileStats where
  fileCompleted (TorrentFileStts (x, _, _)) = x
instance FileCompleted TorrentFile where
  fileCompleted (TorrentFileData (x, _, _)) = x

-- | A mode where the settings for the torrent should be fetched from.
data SettingsMode = ModeDisabled | ModeTorrent | ModeSession deriving (Eq, Enum, Show, Typeable)

instance ToJSON SettingsMode where
  toJSON = toJSON . fromEnum
instance FromJSON SettingsMode where
  parseJSON = fmap toEnum . parseJSON

-- | A standard 'DownloadDirectory' for used when no specific directory should be used.
-- In the current version this is a directory "unsorted" in the default directory for downloads.
unspecifiedDirectory :: DownloadDirectory
unspecifiedDirectory = DownloadDirectory ["unsorted"]

-- | A directory path where downloads are to be placed.
newtype DownloadDirectory = DownloadDirectory [String] deriving (Show, Eq, Typeable)

-- | A list of directories where the downloads are to be placed. If not absolute this is relative to the default downloads directory specified by Transmission.
downloadTo :: [String] -> DownloadDirectory
downloadTo = DownloadDirectory

instance ToJSON DownloadDirectory where
  toJSON (DownloadDirectory ss) = toJSON $ P.intersperse "/" ss

-- | Yeah, I don't really know what this is. Might be epoch in ms (or s)? TODO: CHECK THIS OUT!
newtype TorrentDate = TorrentDate Int deriving (Eq, Show, Typeable)
torrentDate :: TorrentDate -> UTCTime
torrentDate (TorrentDate x) = posixSecondsToUTCTime $ realToFrac x

instance FromJSON TorrentDate where
  parseJSON x = TorrentDate <$> parseJSON x
instance ToJSON TorrentDate where
  toJSON (TorrentDate x) = toJSON x

-- | A field of information about a torrent. The field is accompanied with the type of that field to make some compile-time guarantees by the type system.
data TorrentField t :: * where
  TorrentActivityDate              :: TorrentField TorrentDate
  TorrentAddedDate                 :: TorrentField TorrentDate
  TorrentBandwidthPriority         :: TorrentField BandwidthPriority
  TorrentComment                   :: TorrentField Text
  TorrentCorruptEver               :: TorrentField Int
  TorrentCreator                   :: TorrentField Text
  TorrentDateCreated               :: TorrentField TorrentDate
  TorrentDesiredAvailable          :: TorrentField Int
  TorrentDoneDate                  :: TorrentField TorrentDate
  TorrentDownloadDir               :: TorrentField Text
  TorrentDownloadedEver            :: TorrentField Int
  TorrentDownloadLimit             :: TorrentField SpeedLimit
  TorrentDownloadLimited           :: TorrentField Bool
  TorrentError                     :: TorrentField Int
  TorrentErrorString               :: TorrentField Text
  TorrentETA                       :: TorrentField Int
  TorrentETAIdle                   :: TorrentField Int
  TorrentFiles                     :: TorrentField TorrentFiles
  TorrentFileStats                 :: TorrentField TorrentStatsList
  TorrentFilesWanted               :: TorrentField TorrentFileIds
  TorrentFilesUnwanted             :: TorrentField TorrentFileIds
  TorrentHashString                :: TorrentField Text
  TorrentHaveUnchecked             :: TorrentField Int
  TorrentHaveValid                 :: TorrentField Int
  TorrentHonorsSessionLimits       :: TorrentField Bool
  TorrentId                        :: TorrentField Int
  TorrentIsFinished                :: TorrentField Bool
  TorrentIsPrivate                 :: TorrentField Bool
  TorrentIsStalled                 :: TorrentField Bool
  TorrentLeftUntilDone             :: TorrentField Int
  TorrentLocation                  :: TorrentField Text
  TorrentMagnetLink                :: TorrentField Text
  TorrentManualAnnounceTime        :: TorrentField Int
  TorrentMaxConnectedPeers         :: TorrentField Int
  TorrentMetadataPercentComplete   :: TorrentField Double
  TorrentName                      :: TorrentField Text
  TorrentPeerLimit                 :: TorrentField Int
  -- TorrentPeers                     :: TorrentField TorrentPeers -- TODO add 'peers' support
  TorrentPeersConnected            :: TorrentField Int
  -- TorrentPeersFrom -- TODO add 'peersFrom' support
  TorrentPeersGettingFromUs        :: TorrentField Int
  TorrentPeersSendingToUs          :: TorrentField Int
  TorrentPercentDone               :: TorrentField Double
  -- TorrentPieces -- TODO add 'pieces' support
  TorrentPieceCount                :: TorrentField Int
  TorrentPieceSize                 :: TorrentField Int
  TorrentPriorities                :: TorrentField [TorrentFilePriority]
  TorrentPriorityHigh              :: TorrentField TorrentFileIds
  TorrentPriorityNormal            :: TorrentField TorrentFileIds
  TorrentPriorityLow               :: TorrentField TorrentFileIds
  TorrentQueuePosition             :: TorrentField Int
  TorrentRateDownload              :: TorrentField Int
  TorrentRateUpload                :: TorrentField Int
  TorrentRecheckProgress           :: TorrentField Double
  TorrentSecondsDownloading        :: TorrentField Int
  TorrentSecondsSeeding            :: TorrentField Int
  TorrentSeedIdleLimit             :: TorrentField SeedIdleLimit
  TorrentSeedIdleMode              :: TorrentField SettingsMode
  TorrentSeedRatioLimit            :: TorrentField SeedRatioLimit
  TorrentSeedRatioMode             :: TorrentField SettingsMode
  TorrentSizeWhenDone              :: TorrentField Int64
  TorrentStartDate                 :: TorrentField TorrentDate
  TorrentStatus                    :: TorrentField TorrentStatus
  TorrentTrackerAdd                :: TorrentField Text
  TorrentTrackerRemove             :: TorrentField TrackerId
  TorrentTrackers                  :: TorrentField [TorrentTracker]
  TorrentTrackerStats              :: TorrentField [TorrentTrackerStats]
  TorrentTotalSize                 :: TorrentField Int64
  TorrentFile                      :: TorrentField Text
  TorrentUploadedEver              :: TorrentField Int64
  TorrentUploadLimit               :: TorrentField Int
  TorrentUploadLimited             :: TorrentField Bool
  TorrentUploadRatio               :: TorrentField Double
  TorrentWanted                    :: TorrentField [Bool]
  TorrentWebseeds                  :: TorrentField [Text]
  TorrentWebseedsSendingToUs       :: TorrentField Int
  deriving (Typeable)

-- | The current status of a torrent.
data TorrentStatus = TorrentStatusCheckWait | TorrentStatusChecking | TorrentStatusDownloading | TorrentStatusSeeding | TorrentStatusFinished deriving (Eq, Show, Typeable)
instance ToJSON TorrentStatus where
  toJSON TorrentStatusCheckWait   = JSON.Number 1
  toJSON TorrentStatusChecking    = JSON.Number 2
  toJSON TorrentStatusDownloading = JSON.Number 4
  toJSON TorrentStatusSeeding     = JSON.Number 8
  toJSON TorrentStatusFinished    = JSON.Number 16
instance FromJSON TorrentStatus where
  parseJSON (JSON.Number x) = case x of
    1  -> return TorrentStatusCheckWait
    2  -> return TorrentStatusChecking
    4  -> return TorrentStatusDownloading
    8  -> return TorrentStatusSeeding
    16 -> return TorrentStatusFinished
    _  -> mzero
  parseJSON _ = mzero

data AnyTorrentField = forall t. (Typeable t, Responsable t) => AnyTorrentField (TorrentField t)
anyTorrentField :: AnyTorrentField -> (forall t. (FromJSON t, Typeable t, Responsable t) => TorrentField t)
anyTorrentField x = case anyTorrentField' x of
  Nothing -> error "anyTorrentField: conversion failed."
  Just x' -> x'
--anyTorrentField' :: (FromJSON t, Typeable t, Responsable t) => AnyTorrentField -> Maybe (forall t. TorrentField t)
anyTorrentField' :: forall t. (FromJSON t, Typeable t, Responsable t) => AnyTorrentField -> Maybe (TorrentField t)
anyTorrentField' (AnyTorrentField x) = (fromDynamic . toDyn) x

mapAllTorrentFields :: (forall t. TorrentField t -> b) -> [b]
mapAllTorrentFields f = 
  [ f TorrentActivityDate, f TorrentAddedDate, f TorrentBandwidthPriority
  , f TorrentComment, f TorrentCorruptEver, f TorrentCreator, f TorrentDateCreated
  , f TorrentDesiredAvailable, f TorrentDoneDate, f TorrentDownloadDir
  , f TorrentDownloadedEver, f TorrentDownloadLimit, f TorrentDownloadLimited
  , f TorrentError, f TorrentErrorString, f TorrentETA, f TorrentETAIdle
  , f TorrentFiles, f TorrentFileStats, f TorrentFilesWanted, f TorrentFilesUnwanted
  , f TorrentHashString, f TorrentHaveUnchecked, f TorrentHaveValid
  , f TorrentHonorsSessionLimits, f TorrentId
  , f TorrentIsFinished, f TorrentIsPrivate, f TorrentIsStalled, f TorrentLeftUntilDone
  , f TorrentLocation, f TorrentMagnetLink, f TorrentManualAnnounceTime
  , f TorrentMaxConnectedPeers, f TorrentMetadataPercentComplete, f TorrentName
  , f TorrentPeerLimit --, f TorrentPeers --, f TorentPeersFrom
  , f TorrentPeersConnected, f TorrentPeersGettingFromUs, f TorrentPeersSendingToUs
  , f TorrentPeersSendingToUs, f TorrentPercentDone --, f TorrentPieces
  , f TorrentPieceCount, f TorrentPieceSize, f TorrentQueuePosition
  , f TorrentRateDownload, f TorrentRateUpload, f TorrentRecheckProgress
  , f TorrentSecondsDownloading, f TorrentSecondsSeeding, f TorrentSeedIdleLimit
  , f TorrentSeedIdleMode, f TorrentSeedRatioLimit
  , f TorrentSeedRatioMode, f TorrentSizeWhenDone, f TorrentStartDate
  , f TorrentStatus, f TorrentTrackers, f TorrentTrackerStats
  , f TorrentTrackerAdd, f TorrentTrackerRemove --, f TorrentTrackerReplace
  , f TorrentTotalSize, f TorrentFile, f TorrentUploadedEver
  , f TorrentUploadLimit, f TorrentUploadLimited, f TorrentUploadRatio , f TorrentWanted
  , f TorrentWebseeds, f TorrentWebseedsSendingToUs
  ]


--instance Eq AnyTorrentField
--instance Ord AnyTorrentField

instance Ord (TorrentField t)

instance Eq (TorrentField t) --where
--  TorrentActivityDate == TorrentActivityDate = True
--  _ == _ = False

--type AnyTorrentField = forall t. TorrentField t

-- | A number of 'TorrentField's with associated values.
newtype TorrentFields = TorrentFields (M Text Value) deriving (Eq, Show, Typeable)
-- | A number of 'TorrentField's to be requested from Transmission.
newtype TorrentFieldsRequest = TorrentFieldsRequest [Text] deriving (Eq, Show, Typeable)

-- | Adds a torrent field to a request.
(+|+) :: (Responsable t) => TorrentFieldsRequest -> TorrentField t -> TorrentFieldsRequest
(+|+) (TorrentFieldsRequest xs) x = TorrentFieldsRequest (torrentFieldName x : xs)

-- | Alias for '(+|+)'.
addRequest :: (Responsable t) => TorrentFieldsRequest -> TorrentField t -> TorrentFieldsRequest
addRequest (TorrentFieldsRequest xs) x = TorrentFieldsRequest (torrentFieldName x : xs)

-- | Create a request with a single field.
makeFieldsRequest :: (Responsable t) => TorrentField t -> TorrentFieldsRequest
makeFieldsRequest x = TorrentFieldsRequest [torrentFieldName x]

-- | Test if a 'TorrentField' is able to be set. (That is; it may be used with 'TorrentSet'.)
fieldSettable :: TorrentField t -> Bool
fieldSettable TorrentBandwidthPriority   = True
fieldSettable TorrentDownloadLimit       = True
fieldSettable TorrentDownloadLimited     = True
fieldSettable TorrentFilesWanted         = True
fieldSettable TorrentFilesUnwanted       = True
fieldSettable TorrentHonorsSessionLimits = True
fieldSettable TorrentLocation            = True
fieldSettable TorrentPeerLimit           = True
fieldSettable TorrentPriorityHigh        = True
fieldSettable TorrentPriorityLow         = True
fieldSettable TorrentPriorityNormal      = True
fieldSettable TorrentQueuePosition       = True
fieldSettable TorrentSeedIdleLimit       = True
fieldSettable TorrentSeedIdleMode        = True
fieldSettable TorrentSeedRatioLimit      = True
fieldSettable TorrentSeedRatioMode       = True
fieldSettable TorrentTrackerAdd          = True
fieldSettable TorrentTrackerRemove       = True
--fieldSettable TorrentTrackerReplace      = True -- TODO: TrackerReplace
fieldSettable TorrentUploadLimit         = True
fieldSettable TorrentUploadLimited       = True
fieldSettable _ = False

-- | Test if a 'TorrentField' is able to get. (That is; it may be used with 'TorrentGet'.)
fieldGettable :: (TorrentField t) -> Bool
fieldGettable TorrentActivityDate        = True
fieldGettable TorrentAddedDate           = True
fieldGettable TorrentBandwidthPriority   = True
fieldGettable TorrentComment             = True
fieldGettable TorrentCorruptEver         = True
fieldGettable TorrentCreator             = True
fieldGettable TorrentDateCreated         = True
fieldGettable TorrentDesiredAvailable    = True
fieldGettable TorrentDoneDate            = True
fieldGettable TorrentDownloadDir         = True
fieldGettable TorrentDownloadedEver      = True
fieldGettable TorrentDownloadLimit       = True
fieldGettable TorrentDownloadLimited     = True
fieldGettable TorrentError               = True
fieldGettable TorrentErrorString         = True
fieldGettable TorrentETA                 = True
fieldGettable TorrentETAIdle             = True
fieldGettable TorrentFiles               = True
fieldGettable TorrentFileStats           = True
fieldGettable TorrentFilesWanted         = False
fieldGettable TorrentFilesUnwanted       = False
fieldGettable TorrentHashString          = True
fieldGettable TorrentHaveUnchecked       = True
fieldGettable TorrentHaveValid           = True
fieldGettable TorrentHonorsSessionLimits = True
fieldGettable TorrentId                  = True
fieldGettable TorrentIsFinished          = True
fieldGettable TorrentIsPrivate           = True
fieldGettable TorrentIsStalled           = True
fieldGettable TorrentLeftUntilDone       = True
fieldGettable TorrentLocation            = False
fieldGettable TorrentMagnetLink          = True
fieldGettable TorrentManualAnnounceTime  = True
fieldGettable TorrentMaxConnectedPeers   = True
fieldGettable TorrentMetadataPercentComplete   = True
fieldGettable TorrentName                = True
fieldGettable TorrentPeerLimit           = True
--fieldGettable TorrentPeers               = True -- TODO: Peers
fieldGettable TorrentPeersConnected      = True
--fieldGettable TorrentPeersFrom           = True -- TODO: PeersFrom
fieldGettable TorrentPeersGettingFromUs  = True
fieldGettable TorrentPeersSendingToUs    = True
fieldGettable TorrentPercentDone         = True
--fieldGettable TorrentPieces              = True -- TODO: Pieces
fieldGettable TorrentPieceCount          = True
fieldGettable TorrentPieceSize           = True
fieldGettable TorrentPriorities          = True
fieldGettable TorrentPriorityHigh        = False
fieldGettable TorrentPriorityLow         = False
fieldGettable TorrentPriorityNormal      = False
fieldGettable TorrentQueuePosition       = True
fieldGettable TorrentRateDownload        = True
fieldGettable TorrentRateUpload          = True
fieldGettable TorrentRecheckProgress     = True
fieldGettable TorrentSecondsDownloading  = True
fieldGettable TorrentSecondsSeeding      = True
fieldGettable TorrentSeedIdleLimit       = True
fieldGettable TorrentSeedIdleMode        = True
fieldGettable TorrentSeedRatioLimit      = True
fieldGettable TorrentSeedRatioMode       = True
fieldGettable TorrentSizeWhenDone        = True
fieldGettable TorrentStartDate           = True
fieldGettable TorrentStatus              = True
fieldGettable TorrentTrackers            = True
fieldGettable TorrentTrackerStats        = True
fieldGettable TorrentTrackerAdd          = False
fieldGettable TorrentTrackerRemove       = False
--fieldGettable TorrentTrackerReplace      = False -- TODO: TrackerReplace
fieldGettable TorrentTotalSize           = True
fieldGettable TorrentFile                = True
fieldGettable TorrentUploadedEver        = True
fieldGettable TorrentUploadLimit         = True
fieldGettable TorrentUploadLimited       = True
fieldGettable TorrentUploadRatio         = True
fieldGettable TorrentWanted              = True
fieldGettable TorrentWebseeds            = True
fieldGettable TorrentWebseedsSendingToUs = True
-- MUST BE EXAUSTABLE

--absolutlyAllTorrentFields :: [AnyTorrentField]
--absolutlyAllTorrentFields = mapAllTorrentFields AnyTorrentField
-- | A request asking for all fields possible.
allTorrentFields :: TorrentFieldsRequest
allTorrentFields = TorrentFieldsRequest $ catMaybes $ mapAllTorrentFields f
  where --f :: (Responsable t) => (forall t. TorrentField t -> Maybe Text)
        f x = if fieldGettable x then Just (torrentFieldName x) else Nothing
                   -- (torrentFieldName . anyTorrentField) $ P.filter (fieldGettable . anyTorrentField) absolutlyAllTorrentFields
{-
  TorrentFieldsRequest
  [ torrentFieldName TorrentActivityDate
  , torrentFieldName TorrentAddedDate
  , torrentFieldName TorrentBandwidthPriority
  , torrentFieldName TorrentComment
  , torrentFieldName TorrentCorruptEver
  , torrentFieldName TorrentCreator
  , torrentFieldName TorrentDateCreated
  , torrentFieldName TorrentDesiredAvailable
  , torrentFieldName TorrentDoneDate
  , torrentFieldName TorrentDownloadDir
  , torrentFieldName TorrentDownloadedEver
  , torrentFieldName TorrentDownloadLimit
  , torrentFieldName TorrentDownloadLimited
  , torrentFieldName TorrentError
  , torrentFieldName TorrentErrorString
  , torrentFieldName TorrentETA
  , torrentFieldName TorrentETAIdle
  , torrentFieldName TorrentFiles
  , torrentFieldName TorrentFileStats
  , torrentFieldName TorrentFilesWanted
  , torrentFieldName TorrentFilesUnwanted
  , torrentFieldName TorrentHashString
  , torrentFieldName TorrentHaveUnchecked
  , torrentFieldName TorrentHaveValid
  , torrentFieldName TorrentHonorsSessionLimits
  , torrentFieldName TorrentId
  , torrentFieldName TorrentIsFinished
  , torrentFieldName TorrentIsPrivate
  , torrentFieldName TorrentIsStalled
  , torrentFieldName TorrentLeftUntilDone
  , torrentFieldName TorrentMagnetLink
  , torrentFieldName TorrentManualAnnounceTime
  , torrentFieldName TorrentMaxConnectedPeers
  , torrentFieldName TorrentMetadataPercentComplete
  , torrentFieldName TorrentName
  , torrentFieldName TorrentPeerLimit
--  , torrentFieldName TorrentPeers
  , torrentFieldName TorrentPeersConnected
  , torrentFieldName TorrentPeersGettingFromUs
  , torrentFieldName TorrentPeersSendingToUs
  , torrentFieldName TorrentPercentDone
--  , torrentFieldName TorrentPieces
  , torrentFieldName TorrentPieceCount
  , torrentFieldName TorrentPieceSize
  , torrentFieldName TorrentQueuePosition
  , torrentFieldName TorrentRateDownload
  , torrentFieldName TorrentRateUpload
  , torrentFieldName TorrentRecheckProgress
  , torrentFieldName TorrentSecondsDownloading
  , torrentFieldName TorrentSecondsSeeding
  , torrentFieldName TorrentSeedIdleLimit
  , torrentFieldName TorrentSeedIdleMode
  , torrentFieldName TorrentSeedRatioLimit
  , torrentFieldName TorrentSeedRatioMode
  , torrentFieldName TorrentSizeWhenDone
  , torrentFieldName TorrentStartDate
  , torrentFieldName TorrentStatus
  , torrentFieldName TorrentTotalSize
  , torrentFieldName TorrentFile
  , torrentFieldName TorrentUploadedEver
  , torrentFieldName TorrentUploadLimit
  , torrentFieldName TorrentUploadLimited
  , torrentFieldName TorrentUploadRatio
  , torrentFieldName TorrentWebseedsSendingToUs
  ]
--}

instance ToJSON TorrentFields where
  toJSON (TorrentFields v) = JSON.Object v

instance FromJSON TorrentFields where
  parseJSON (JSON.Object v) = return $ TorrentFields v -- TODO be a bit stricter perhaps?
  parseJSON _ = mzero

--requestFields :: [AnyTorrentField] -> TorrentFieldsRequest
--requestFields = TorrentFieldsRequest

--class JSONValue t where
--  fromJSONValue :: Value -> Maybe t

{-
instance FromJSON TorrentStatus where
  parseJSON (JSON.Number r) = flip fmap (toBoundedInteger r) $ \x -> case (x :: Int) of
    0x01 -> return TorrentStatusCheckWait
    0x02 -> return TorrentStatusChecking
    0x04 -> return TorrentStatusDownloading
    0x08 -> return TorrentStatusSeeding
    0x10 -> return TorrentStatusFinished
    _    -> mzero
  parseJSON _ = mzero
instance FromJSON Int where
  parseJSON (JSON.Number r) = case toBoundedInteger r of
    Just r' -> return r'
    _       -> mzero
  parseJSON _ = mzero
instance FromJSON Double where
  parseJSON (JSON.Number r) = return $ toRealFloat r
  parseJSON _ = mzero
instance FromJSON Text where
  parseJSON (JSON.String t) =  t
  parseJSON _ = Nothing
instance JSONValue Bool where
  fromJSONValue (JSON.Bool t) = Just t
  fromJSONValue _ = Nothing
instance (JSONValue t) => JSONValue (Maybe t) where
  fromJSONValue JSON.Null = Just Nothing
  fromJSONValue t         = fmap Just $ fromJSONValue t
-}

-- | Find the content of a specific field.
lookupTorrentField :: forall t. (Responsable t) => TorrentField t -> TorrentFields -> Maybe t
lookupTorrentField k (TorrentFields m) = case M.lookup (torrentFieldName k) m of
  Nothing -> Nothing
  Just r' -> parseMaybe rawResponse' r'

-- | A TorrentInfoList is a number of (>= 0) torrents represented by 'TorrentFields'. These may be extracted using 'torrentInfoToList'.
newtype TorrentInfoList = TorrentInfoList [TorrentFields] deriving (Eq, Show, Typeable)
instance ToJSON TorrentInfoList where
  toJSON (TorrentInfoList v) = toJSON v
instance FromJSON TorrentInfoList where
  parseJSON v = TorrentInfoList <$> parseJSON v
instance Monoid TorrentInfoList where
  mappend (TorrentInfoList a) (TorrentInfoList b) = TorrentInfoList $ mappend a b
  mempty = TorrentInfoList []
  mconcat = TorrentInfoList . mconcat . P.map (\(TorrentInfoList a) -> a)

-- | Extract the 'TorrentInfoList' to a list of 'TorrentFields'.
torrentInfoToList :: TorrentInfoList -> [TorrentFields]
torrentInfoToList (TorrentInfoList a) = a

-- | A request to be sent to Transmission. Includes the type of the expected return value.
data Request t where
  TorrentStart    :: TorrentList -> Request ()
  TorrentStop     :: TorrentList -> Request ()
  TorrentVerify   :: TorrentList -> Request ()
  TorrentSet      :: TorrentList -> TorrentFields -> Request ()
  TorrentGet      :: TorrentList -> TorrentFieldsRequest -> Request TorrentInfoList
  TorrentAdd      :: Text -> DownloadDirectory -> Maybe Cookies -> Request (Either TorrentDuplicate TorrentAdded)

-- | A general request as to be sent to Transmission.
data RawRequest = RawReq { rawMethod :: Text, rawArguments :: Value, rawTag :: Tag }
instance ToJSON RawRequest where
  toJSON rr = object $ [ "method" .= rawMethod rr, "arguments" .= rawArguments rr, "tag" .= rawTag rr ]

-- | A general response as received from Transmission.
data RawResponse = RawResp { rawError :: Maybe Text, rawResponseTag :: Maybe Tag, responseObject :: Maybe Value }
instance FromJSON RawResponse where
  parseJSON (Object v) = do
    err <- v .: "result"
    tag <- (Just <$> (v .: "tag")) <|> return Nothing
    obj <- (Just <$> (v .: "arguments")) <|> return Nothing
    return $ RawResp { rawError = if err == "success" then Nothing else Just err, rawResponseTag = tag, responseObject = obj }
  parseJSON _ = mzero

-- | A 'TorrentList' representing all torrents.
allTorrents :: TorrentList
allTorrents = Nothing

-- | Any container which contains 'TorrentFields'.
class TorrentFieldsContainer t where
  torrentFields :: t -> TorrentFields
  torrentFieldsToList :: t -> [(Text, JSON.Value)]
  torrentFieldsToList x = case torrentFields x of
    TorrentFields y -> M.toList y

instance TorrentFieldsContainer TorrentFields where
  torrentFields = id
  torrentFieldsToList (TorrentFields x) = M.toList x

instance TorrentFieldsContainer TorrentAdded where
  torrentFields (TorrentAdded x) = x
instance TorrentFieldsContainer TorrentDuplicate where
  torrentFields (TorrentDuplicate x) = x

-- | Response when a torrent has been successfully added. Use 'torrentFields' in combination with 'lookupTorrentField' to get more info about the torrent.
newtype TorrentAdded = TorrentAdded TorrentFields deriving (Eq, Show, Typeable)

-- | Response when a torrent attempted to be added but it already existed. Use 'torrentFields' in combination with 'lookupTorrentField' to get more info about the torrent.
newtype TorrentDuplicate = TorrentDuplicate TorrentFields deriving (Eq, Show, Typeable)

instance FromJSON TorrentAdded where
  parseJSON a = TorrentAdded <$> parseJSON a
instance ToJSON TorrentAdded where
  toJSON (TorrentAdded a) = toJSON a

instance FromJSON TorrentDuplicate where
  parseJSON a = TorrentDuplicate <$> parseJSON a
instance ToJSON TorrentDuplicate where
  toJSON (TorrentDuplicate a) = toJSON a

-- | Sets a specific field in a collection of torrent fields.
setTorrentField :: (Responsable t) => TorrentField t -> t -> TorrentFields -> TorrentFields
setTorrentField f v (TorrentFields m) = TorrentFields $ M.insert (torrentFieldName f) (responseJSONValue v) m

-- | The textual name for a field as used in communication with Transmission.
torrentFieldName :: TorrentField t -> Text
torrentFieldName TorrentActivityDate              = "activityDate"
torrentFieldName TorrentAddedDate                 = "addedDate"
torrentFieldName TorrentBandwidthPriority         = "bandwidthPriority"
torrentFieldName TorrentComment                   = "comment"
torrentFieldName TorrentCorruptEver               = "corruptEver"
torrentFieldName TorrentCreator                   = "creator"
torrentFieldName TorrentDateCreated               = "dateCreated"
torrentFieldName TorrentDesiredAvailable          = "desiredAvailable"
torrentFieldName TorrentDoneDate                  = "doneDate"
torrentFieldName TorrentDownloadDir               = "downloadDir"
torrentFieldName TorrentDownloadedEver            = "downloadedEver"
torrentFieldName TorrentDownloadLimit             = "downloadLimit"
torrentFieldName TorrentDownloadLimited           = "downloadLimited"
torrentFieldName TorrentError                     = "error"
torrentFieldName TorrentErrorString               = "errorString"
torrentFieldName TorrentETA                       = "eta"
torrentFieldName TorrentETAIdle                   = "etaIdle"
torrentFieldName TorrentFiles                     = "files"
torrentFieldName TorrentFileStats                 = "fileStats"
torrentFieldName TorrentFilesWanted               = "files-wanted"
torrentFieldName TorrentFilesUnwanted             = "files-unwanted"
torrentFieldName TorrentHashString                = "hashString"
torrentFieldName TorrentHaveUnchecked             = "haveUnchecked"
torrentFieldName TorrentHaveValid                 = "haveValid"
torrentFieldName TorrentHonorsSessionLimits       = "honorsSessionLimits"
torrentFieldName TorrentId                        = "id"
torrentFieldName TorrentIsFinished                = "isFinished"
torrentFieldName TorrentIsPrivate                 = "isPrivate"
torrentFieldName TorrentIsStalled                 = "isStalled"
torrentFieldName TorrentLeftUntilDone             = "leftUntilDone"
torrentFieldName TorrentLocation                  = "location"
torrentFieldName TorrentMagnetLink                = "magnetLink"
torrentFieldName TorrentManualAnnounceTime        = "manualAnnounceTime"
torrentFieldName TorrentMaxConnectedPeers         = "maxConnectedPeers"
torrentFieldName TorrentMetadataPercentComplete   = "metadataPercentComplete"
torrentFieldName TorrentName                      = "name"
torrentFieldName TorrentPeerLimit                 = "peer-limit"
torrentFieldName TorrentPeersConnected            = "peersConnected"
torrentFieldName TorrentPeersGettingFromUs        = "peersGettingFromUs"
torrentFieldName TorrentPeersSendingToUs          = "peersSendingToUs"
torrentFieldName TorrentPercentDone               = "percentDone"
torrentFieldName TorrentPieceCount                = "pieceCount"
torrentFieldName TorrentPieceSize                 = "pieceSize"
torrentFieldName TorrentPriorities                = "priorities"
torrentFieldName TorrentPriorityHigh              = "priority-high"
torrentFieldName TorrentPriorityLow               = "priority-low"
torrentFieldName TorrentPriorityNormal            = "priority-normal"
torrentFieldName TorrentQueuePosition             = "queuePosition"
torrentFieldName TorrentRateDownload              = "rateDownload"
torrentFieldName TorrentRateUpload                = "rateUpload"
torrentFieldName TorrentRecheckProgress           = "recheckProgress"
torrentFieldName TorrentSecondsDownloading        = "secondsDownloading"
torrentFieldName TorrentSecondsSeeding            = "secondsSeeding"
torrentFieldName TorrentSeedIdleLimit             = "seedIdleLimit"
torrentFieldName TorrentSeedIdleMode              = "seedIdleMode"
torrentFieldName TorrentSeedRatioLimit            = "seedIdleRatioLimit"
torrentFieldName TorrentSeedRatioMode             = "seedIdleRatioMode"
torrentFieldName TorrentSizeWhenDone              = "sizeWhenDone"
torrentFieldName TorrentStartDate                 = "startDate"
torrentFieldName TorrentStatus                    = "status"
torrentFieldName TorrentTrackers                  = "trackers"
torrentFieldName TorrentTrackerStats              = "trackerStats"
torrentFieldName TorrentTrackerAdd                = "trackerAdd"
torrentFieldName TorrentTrackerRemove             = "trackerRemove"
--torrentFieldName TorrentTrackerReplace            = "trackerReplace"
torrentFieldName TorrentTotalSize                 = "totalSize"
torrentFieldName TorrentFile                      = "torrentFile"
torrentFieldName TorrentUploadedEver              = "uploadedEver"
torrentFieldName TorrentUploadLimit               = "uploadLimit"
torrentFieldName TorrentUploadLimited             = "uploadLimited"
torrentFieldName TorrentUploadRatio               = "uploadRatio"
torrentFieldName TorrentWanted                    = "wanted"
torrentFieldName TorrentWebseeds                  = "webseeds"
torrentFieldName TorrentWebseedsSendingToUs       = "webseedsSendingToUs"

{-
torrentFieldFromName :: Text -> Maybe (TorrentField t)
torrentFieldFromName "activityDate" = Just TorrentActivityDate
torrentFieldFromName "addedDate" = Just TorrentAddedDate
torrentFieldFromName "bandwidthPriority" = Just TorrentBandwidthPriority
torrentFieldFromName "comment" = Just TorrentComment
torrentFieldFromName "corruptEver" = Just TorrentCorruptEver
torrentFieldFromName "creator" = Just TorrentCreator
torrentFieldFromName "dateCreated" = Just TorrentDateCreated
torrentFieldFromName "desiredAvailable" = Just TorrentDesiredAvailable
torrentFieldFromName "doneDate" = Just TorrentDoneDate
torrentFieldFromName "downloadDir" = Just TorrentDownloadDir
torrentFieldFromName "downloadedEver" = Just TorrentDownloadedEver
torrentFieldFromName "downloadLimit" = Just TorrentDownloadLimit
torrentFieldFromName "downloadLimited" = Just TorrentDownloadLimited
torrentFieldFromName "error" = Just TorrentError
torrentFieldFromName "errorString" = Just TorrentErrorString
torrentFieldFromName "eta" = Just TorrentETA
torrentFieldFromName "etaIdle" = Just TorrentETAIdle
torrentFieldFromName "files" = Just TorrentFiles
torrentFieldFromName "fileStats" = Just TorrentFileStats
torrentFieldFromName "files-wanted" = Just TorrentFilesWanted
torrentFieldFromName "files-unwanted" = Just TorrentFilesUnwanted
torrentFieldFromName "hashString" = Just TorrentHashString
torrentFieldFromName "haveUnchecked" = Just TorrentHaveUnchecked
torrentFieldFromName "haveValid" = Just TorrentHaveValid
torrentFieldFromName "honorsSessionLimits" = Just TorrentHonorsSessionLimits
torrentFieldFromName "id" = Just TorrentId
torrentFieldFromName "isFinished" = Just TorrentIsFinished
torrentFieldFromName "isPrivate" = Just TorrentIsPrivate
torrentFieldFromName "isStalled" = Just TorrentIsStalled
torrentFieldFromName "leftUntilDone" = Just TorrentLeftUntilDone
torrentFieldFromName "location" = Just TorrentLocation
torrentFieldFromName "magnetLink" = Just TorrentMagnetLink
torrentFieldFromName "manualAnnounceTime" = Just TorrentManualAnnounceTime
torrentFieldFromName "maxConnectedPeers" = Just TorrentMaxConnectedPeers
torrentFieldFromName "metadataPercentComplete" = Just TorrentMetadataPercentComplete
torrentFieldFromName "name" = Just TorrentName
torrentFieldFromName "peer-limit" = Just TorrentPeerLimit
torrentFieldFromName "peersConnected" = Just TorrentPeersConnected
torrentFieldFromName "peersGettingFromUs" = Just TorrentPeersGettingFromUs
torrentFieldFromName "peersSendingToUs" = Just TorrentPeersSendingToUs
torrentFieldFromName "percentDone" = Just TorrentPercentDone
torrentFieldFromName "pieceCount" = Just TorrentPieceCount
torrentFieldFromName "pieceSize" = Just TorrentPieceSize
torrentFieldFromName "priority-high" = Just TorrentPriorityHigh
torrentFieldFromName "priority-low" = Just TorrentPriorityLow
torrentFieldFromName "priority-normal" = Just TorrentPriorityNormal
torrentFieldFromName "queuePosition" = Just TorrentQueuePosition
torrentFieldFromName "rateDownload" = Just TorrentRateDownload
torrentFieldFromName "rateUpload" = Just TorrentRateUpload
torrentFieldFromName "recheckProgress" = Just TorrentRecheckProgress
torrentFieldFromName "secondsDownloading" = Just TorrentSecondsDownloading
torrentFieldFromName "secondsSeeding" = Just TorrentSecondsSeeding
torrentFieldFromName "seedIdleLimit" = Just TorrentSeedIdleLimit
torrentFieldFromName "seedIdleMode" = Just TorrentSeedIdleMode
torrentFieldFromName "seedIdleRatioLimit" = Just TorrentSeedRatioLimit
torrentFieldFromName "seedIdleRatioMode" = Just TorrentSeedRatioMode
torrentFieldFromName "sizeWhenDone" = Just TorrentSizeWhenDone
torrentFieldFromName "startDate" = Just TorrentStartDate
torrentFieldFromName "status" = Just TorrentStatus
torrentFieldFromName "totalSize" = Just TorrentTotalSize
--    , (AddTracker             , "tracker-add"
--    , (RemoveTracker          , "tracker-remove"
--    , (ReplaceTracker         , "tracker-replace"
torrentFieldFromName "torrentFile" = Just TorrentFile
torrentFieldFromName "uploadedEver" = Just TorrentUploadedEver
torrentFieldFromName "uploadLimit" = Just TorrentUploadLimit
torrentFieldFromName "uploadLimited" = Just TorrentUploadLimited
torrentFieldFromName "uploadRatio" = Just TorrentUploadRatio
torrentFieldFromName "webseedsSendingToUs" = Just TorrentWebseedsSendingToUs
torrentFieldFromName _ = Nothing
-}

-- | Transforms an abstract 'Request' to a more general 'RawRequest' which may be serialized to JSON and sent in communication with Transmission.
rawRequest :: (Responsable t) => TagGenerator -> Request t -> RawRequest
rawRequest f (TorrentStart tl)  = rawRequest' f "torrent-start"  $ object $ case tl of { Nothing -> [] ; Just tl' -> ["ids" .= tl'] }
rawRequest f (TorrentStop tl)   = rawRequest' f "torrent-stop"   $ object $ case tl of { Nothing -> [] ; Just tl' -> ["ids" .= tl'] }
rawRequest f (TorrentVerify tl) = rawRequest' f "torrent-verify" $ object $ case tl of { Nothing -> [] ; Just tl' -> ["ids" .= tl'] }
rawRequest f (TorrentSet tl (TorrentFields sa)) = rawRequest' f "torrent-set" $ JSON.Object $ case tl of { Nothing -> sa ; Just tl' -> M.insert "ids" (toJSON tl') sa }
rawRequest f (TorrentAdd url dir cks) = rawRequest' f "torrent-add" $ object $ (case cks of { Nothing -> id ; Just cks' -> (:) ("cookies" .= joinCookies cks') }) ["filename" .= url, "downloadDir" .= dir ]
rawRequest f (TorrentGet tl (TorrentFieldsRequest fr)) = rawRequest' f "torrent-get" $ JSON.Object $ M.fromList $ (case tl of { Nothing -> id ; Just tl' -> (:) ("ids", toJSON tl') }) [("fields", toJSON fr)]

-- | Create a general 'RawRequest' from a request name and an arguments object.
rawRequest' :: TagGenerator -> Text -> Value -> RawRequest
rawRequest' f t o = RawReq { rawMethod = t, rawArguments = o, rawTag = f t o }

-- | Utility function to join a series of cookies to a single text string.
joinCookies :: Cookies -> Text
joinCookies = T.intercalate "; " . P.map (\(x,y) -> T.append x $ T.cons '=' y)

-- | A session id used for communication with Transmission.
type TransmissionId = Text
-- | A connection to Transmission. Contains the state. Use 'transmissionConnect' to obtain.
newtype TransmissionConnection = TCon (URI, MVar (ByteString -> HTTP.Request ByteString, HTTP.BrowserState (HTTP.HandleStream ByteString)))

-- | Connect to a Transmission daemon running at a known URL.
--
-- Example:
--
-- > con <- transmissionConnect $ parseURI "http://localhost:9091/transmission/rpc"
transmissionConnect :: URI -> IO TransmissionConnection
transmissionConnect uri = do
  s <- HTTP.browse $ do
    (_, r) <- HTTP.request $ rqf uri "" "{\"method\":\"session-stats\"}"
    case HTTP.rspCode r of
     (4,0,9)   -> case HTTP.findHeader sessionHeader r of
       Nothing -> fail "transmissionConnect: Can not find session header."
       Just sh -> (\x -> (rqf uri sh, x)) <$> get
     (2,0,0)   -> (\x -> (rqf uri "", x)) <$> get
     (x,y,z)   -> fail $ "transmissionConnect: unexpected server response: " ++ show x ++ show y ++ show z
  (\x -> TCon (uri, x)) <$> newMVar s

-- | A "requestfunction" used for simplicity when updating a 'TransmissionConnection' with a valid session. This specifies how to actually send the JSON request.
rqf :: URI -> String -> ByteString -> HTTP.Request ByteString
rqf uri cde bdy = HTTP.Request uri HTTP.POST (HTTP.Header HTTP.HdrContentLength (show $ BS.length bdy) : HTTP.Header sessionHeader cde : staticHeaders) bdy
  where
    staticHeaders = [HTTP.Header HTTP.HdrContentType "application/json", HTTP.Header HTTP.HdrAccept "application/json,application/javascript;q=0.1"]

-- | Run some special session or browser action within an active connection. Since this may be used to do anything to affect the connection it is named with the Unsafe postfix.
runBrowserActionUnsafe :: TransmissionConnection -> HTTP.BrowserAction (HTTP.HandleStream ByteString) r -> IO r
runBrowserActionUnsafe (TCon (_, tcon)) act = do
  modifyMVar tcon $ \(post, s) -> do
    HTTP.browse $ HTTP.withBrowserState s $ do
      r <- act
      (\x -> ((post, x), r)) <$> get

-- | The 'HTTP.HeaderName' used to contain the 'TransmissionId'.
sessionHeader :: HTTP.HeaderName
sessionHeader = HTTP.HdrCustom "X-Transmission-Session-Id"

-- | Sends a general 'RawRequest' using an active 'TransmissionConnection' and awaits its response.
sendRawRequest :: TransmissionConnection -> RawRequest -> IO RawResponse
sendRawRequest (TCon (uri, tcon)) rreq = do
  mrr <- modifyMVar tcon $ \(post, s) -> do
    HTTP.browse $ HTTP.withBrowserState s $ do
      (_, r) <- HTTP.request $ post $ encode rreq
      case HTTP.rspCode r of
       (4,0,9)   -> case HTTP.findHeader sessionHeader r of
         Nothing -> fail "transmissionConnect: Can not find session header."
         Just sh -> let post' = rqf uri sh
                    in do (_, r') <- HTTP.request $ post' $ encode rreq
                          case HTTP.rspCode r' of
                           (4,0,9) -> fail $ "sendRawRequest: invalid session found by repeated 409"
                           (2,0,0) -> (\x -> ((post', x), decode (HTTP.rspBody r'))) <$> get
                           (x,y,z) -> fail $ "sendRawRequest: unexpected response code " ++ show x ++ show y ++ show z ++ " during session renewal"
       (2,0,0)   -> (\x -> ((post, x), decode (HTTP.rspBody r))) <$> get
       (x,y,z)   -> fail $ "sendRawRequest: unexpected response code " ++ show x ++ show y ++ show z
  --print (mrr :: Maybe JSON.Value)
  case mrr of
   Nothing -> fail "sendRawRequest: failed to parse response."
   Just rr -> case parseMaybe parseJSON rr of
     Nothing -> fail "sendRawRequest: failed to parse JSON as response."
     Just r' -> return r'

-- | Send a 'Request' and await 'Response'.
sendRequest :: (Responsable t) => TransmissionConnection -> TagGenerator -> Request t -> IO (Response t)
sendRequest tcon tgf req = rawResponse <$> (sendRawRequest tcon $ rawRequest tgf req)

-- | A response to a sent 'Request'. Might result in a 'responseError' or contains a 'responseValue'.
-- The type of the responseValue, if any, is strictly typed as to be the same as the requested type. The typesystem should be able to do much work for us.
data Response t = Response { responseError :: Maybe Text, responseTag :: Maybe Tag, responseValue :: Maybe t } deriving (Show)

{-
class (Eq t, ToJSON t, JSONValue t) => KeyedResponse t where
  keyedParameter :: t
  responseKey :: t -> Text
-}

-- | Helper class to make the typesystem strict but still be able to handle some non trivial JSON responses.
class (Eq t, Typeable t, ResponseType t flag) => Responsable' flag t where
  rawResponse'' :: flag -> Value -> JSON.Parser t
  responseJSONValue' :: flag -> t -> Value

class (Eq t, Typeable t, FromJSON t, ToJSON t) => Responsable t where
  rawResponse :: RawResponse -> Response t
  rawResponse' :: Value -> JSON.Parser t
  responseJSONValue :: t -> Value

instance (FromJSON t, Typeable t, ToJSON t, Responsable' flag t, ResponseType t flag) => Responsable t where
  responseJSONValue = responseJSONValue' (undefined :: flag)
  rawResponse rrsp = Response { responseError = rawError rrsp
                              , responseTag = rawResponseTag rrsp
                              , responseValue = case responseObject rrsp of { Nothing -> Nothing ; Just rsp -> parseMaybe rawResponse' rsp }
                              }
  rawResponse' = rawResponse'' (undefined :: flag)


-- | Helper datatype used for creating the possibility of non trivial JSON responses and still have a strict response type.
data KeyedResponse
-- | Helper datatype used for creating the possibility of non trivial JSON responses and still have a strict response type.
data SimpleResponse
-- | Helper datatype used for creating the possibility of non trivial JSON responses and still have a strict response type.
data NoResponse
-- | Helper datatype used for creating the possibility of non trivial JSON responses and still have a strict response type.
data EitherResponse

-- | Helper class to make the typesystem strict but still be able to handle some non trivial JSON responses.
class (Eq t, Typeable t, FromJSON t, ToJSON t) => ResponseType t flag | t->flag where
  responseTypeValue :: flag -> t -> Text
  responseTypeValue = error "responseTypeValue: no type value to be found"
--  responseTypeContent :: (Response t1) => flag -> t -> t1
  responseTypeUndefined :: t -> flag
  responseTypeUndefined _ = (undefined :: flag)

-- instance TypeCast flag HFalse => ShowPred a flag -- before -XTypeFamilies
--instance (FromJSON t, ToJSON t, flag ~ SimpleResponse) => ResponseType t flag

instance ResponseType Int    SimpleResponse
instance ResponseType Int64  SimpleResponse
instance ResponseType Bool   SimpleResponse
instance ResponseType Text   SimpleResponse
instance ResponseType Double SimpleResponse

instance (Eq t, Typeable t, ToJSON t, FromJSON t) => ResponseType [t] SimpleResponse

instance ResponseType TorrentDate       SimpleResponse
instance ResponseType TorrentStatus     SimpleResponse
instance ResponseType SettingsMode      SimpleResponse
instance ResponseType TorrentFile       SimpleResponse
instance ResponseType TorrentFileIds    SimpleResponse
instance ResponseType TorrentFiles      SimpleResponse
instance ResponseType TorrentFileStats  SimpleResponse
instance ResponseType TorrentStatsList  SimpleResponse

instance ResponseType ()     NoResponse

instance ResponseType TorrentAdded       KeyedResponse where
  responseTypeValue _ _ = "torrent-added"
instance ResponseType TorrentDuplicate   KeyedResponse where
  responseTypeValue _ _ = "torrent-duplicate"
instance ResponseType TorrentInfoList    KeyedResponse where
  responseTypeValue _ _ = "torrents"

instance (ResponseType a flag1, ResponseType b flag2) => ResponseType (Either a b) EitherResponse where

instance (ResponseType t NoResponse, Eq t) => Responsable' NoResponse t where
  responseJSONValue' _ _ = JSON.Null
  rawResponse'' _ _ = mzero

instance (ResponseType t SimpleResponse, Eq t, FromJSON t, ToJSON t) => Responsable' SimpleResponse t where
  responseJSONValue' _ = toJSON
  rawResponse'' _ = parseJSON

instance (ResponseType t KeyedResponse, Eq t, FromJSON t, ToJSON t) => Responsable' KeyedResponse t where
  responseJSONValue' k t = object $ [responseTypeValue k t .= toJSON t]
  rawResponse'' k (JSON.Object t) = case M.lookup (responseTypeValue k (undefined :: t)) t of
    Nothing -> mzero
    Just t' -> parseJSON t'
  rawResponse'' _ _ = mzero

instance (Eq a, Eq b, Responsable' flag1 a, Responsable' flag2 b) => Responsable' EitherResponse (Either a b) where
  responseJSONValue' _ (Left t) = responseJSONValue' (undefined :: flag1) t
  responseJSONValue' _ (Right t) = responseJSONValue' (undefined :: flag2) t
  rawResponse'' _ t = (Right <$> rawResponse'' (undefined :: flag2) t) <|> (Left <$> rawResponse'' (undefined :: flag1) t)


{-
instance Responsable () where
  rawResponse' _ = mzero
  responseJSONValue _ = JSON.Null

instance Responsable Text where
  rawResponse' (JSON.String v) = return v
  rawResponse' _ = mzero
  responseJSONValue _ = ""

instance (Responsable t1, Responsable t2) => Responsable (Either t1 t2) where
  rawResponse' v = (Right <$> rawResponse' v)  <|> (Left <$> rawResponse' v)
  responseJSONValue v = case v of
    Right r -> responseJSONValue r
    Left  l -> responseJSONValue l

instance (KeyedResponse t, ToJSON t, JSONValue t) => Responsable t where
  rawResponse' (Object v) = case M.lookup (responseKey (keyedParameter :: t)) v of
    Nothing -> mzero
    Just v' -> case fromJSONValue v' of
      Nothing -> mzero
      Just vv -> return vv
  rawResponse' _ = mzero
  responseJSONValue v = object $ [(responseKey v, toJSON v)]

instance ToJSON TorrentAdded where
  toJSON (TorrentAdded x) = toJSON x
instance ToJSON TorrentDuplicate where
  toJSON (TorrentDuplicate x) = toJSON x

instance KeyedResponse TorrentAdded where
  keyedParameter = TorrentAdded $ TorrentFields $ M.empty
  responseKey _  = "torrent-added"

instance KeyedResponse TorrentDuplicate where
  keyedParameter = TorrentDuplicate $ TorrentFields $ M.empty
  responseKey _  = "torrent-duplicate"


instance (Responsable t) => JSONValue t where
  fromJSONValue = parseMaybe rawResponse'

-}

-- | Extract the hash from a magnet link.
magnetToHash :: Text -> Maybe TorrentHash
magnetToHash magnet = case T.break ('?' ==) magnet of
  (_, ob) -> if T.null ob then Nothing
             else case (P.map (T.takeWhile ('&' /=)) $ P.filter ("xt=urn:btih:" `T.isPrefixOf`) $ P.map snd $ T.breakOnAll "&" $ T.tail ob) of
                   (x':_) -> Just $ T.drop (T.length "xt=urn:btih:") x'
                   _      -> Nothing
