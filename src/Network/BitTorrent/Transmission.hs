
module Network.BitTorrent.Transmission
       ( -- * Specialized types
         BandwidthPriority, TorrentHash, Tag, TorrentList, TagGenerator, SpeedLimit, FileIndices, PeerLimit, QueuePosition, TrackerAnnounce, TrackerId, SeedIdleLimit, SeedRatioLimit, Cookies,
         SeedIdleMode, SeedRatioMode, TorrentFileId, TorrentFile, TorrentFileStats, TorrentFileIds, TorrentFiles, DownloadDirectory, TorrentDate, TorrentField(..), TorrentStatus(..),
         TorrentFields, TorrentFieldsRequest, TorrentInfoList, Request(..), TorrentAdded, TorrentDuplicate, TransmissionConnection(), Response, TorrentListable,
         -- * type classes
         FileCompleted, SettingsMode, Responsable, TorrentFieldsContainer,
         -- * stuff...
         allTorrentFileIds, fileLength, fileName, fileWanted, filePriority, fileCompleted, unspecifiedDirectory, downloadTo, (+|+), addRequest, makeFieldsRequest, allTorrentFields, lookupTorrentField,
         torrentInfoToList, allTorrents, torrentFields, torrentFieldsToList, setTorrentField, torrentFieldName, transmissionConnect, sendRequest, responseError, responseTag, responseValue, asList,
         torrentDate,
         -- * Applicative torrent properties
         PendingTorrentAction(), TorrentFieldGetter(),
         torrentGet, torrentGetList, getField
       ) where

import Network.BitTorrent.Transmission.Internal
import Control.Applicative
import Control.Monad.State
import Data.Maybe
import qualified Data.Text as T

newtype PendingTorrentAction f t = PendingAction (f -> t)
newtype TorrentGet' = TorrentGet' { fieldRequestsTorrentGet' :: TorrentFieldsRequest }
newtype TorrentFieldGetter t = TorrentGetCons (State TorrentGet' (PendingTorrentAction TorrentFields t))

instance Functor (PendingTorrentAction t) where
  fmap f (PendingAction x) = PendingAction (f . x)
instance Applicative (PendingTorrentAction t) where
  pure x = PendingAction $ const x
  (PendingAction ff) <*> (PendingAction fx) = PendingAction $ \z -> ff z (fx z)



instance Functor TorrentFieldGetter where
  fmap f (TorrentGetCons x) = TorrentGetCons (fmap (\(PendingAction x') -> PendingAction (f . x')) x)

{--
instance Monad TorrentFieldGetter where
  return x = TorrentGetCons $ return (PendingAction $ const x)
  (TorrentGetCons x) >>  (TorrentGetCons y) = TorrentGetCons (x >> y)
  (TorrentGetCons x) >>= fy = TorrentGetCons $ do
    PendingAction x' <- x
    let TorrentGetCons y' = fy x'
    y'
--}
instance Applicative TorrentFieldGetter where
  pure x = TorrentGetCons $ pure (PendingAction $ const x)
  (TorrentGetCons ff) <*> (TorrentGetCons fx) = TorrentGetCons $ do
    PendingAction f <- ff
    PendingAction x <- fx
    return $ PendingAction $ \z -> f z (x z)
  (TorrentGetCons ff) *>  (TorrentGetCons fx) = TorrentGetCons (ff *> fx)
  (TorrentGetCons ff) <*  (TorrentGetCons fx) = TorrentGetCons (ff <* fx)

-- | Applicative version of @'sendRequest' c f $ 'TorrentGet' (Just [hash]) x@.
--
-- This simplifies the usage of getting fields as to lower repetitions of field names and to make the code a bit cleaner.
--
-- >>> let showTorrent connection hexhash = do
-- >>>       r <- torrentGet connection hashhex $
-- >>>            (\hash name -> "TORRENT (" ++ show hash ++ "): " ++ show name) <$> getField TorrentHashString <*> getField TorrentName
-- >>>       putStrLn $ fromMaybe "No torrent with that hash exists." r
-- >>> in showTorrent localTransmission "FC8A15A2FAF2734DBB1DC5F7AFDC5C9BEAEB1F59"
-- TORRENT ("FC8A15A2FAF2734DBB1DC5F7AFDC5C9BEAEB1F59"): "Ubuntu 15.04 Vivid Vervet Desktop amd64 ISO FINAL"
torrentGet :: TransmissionConnection -> TorrentHash -> TorrentFieldGetter a -> IO (Maybe a)
torrentGet con hash m = do
  r <- torrentGetList con (Just [hash]) m
  case r of
   [] -> return Nothing
   [r'] -> return (Just r')
   rrrr -> fail $ "torrentGet: Response to long: " ++ show (length rrrr)

-- | Applicative version of @'sendRequest' c f $ 'TorrentGet' hashes x@. The ordering and existance of all requested Torrents are not guaranteed.
--
-- This simplifies the usage of getting fields as to lower repetitions of field names and to make the code a bit cleaner.
--
-- >>> let showTorrents connection hashes = do
-- >>>       mapM_ putStrLn =<< torrentGetList connection hashes $
-- >>>            (\hash name -> "TORRENT (" ++ show hash ++ "): " ++ show name) <$> getField TorrentHashString <*> getField TorrentName
-- >>> in showTorrents localTransmission $ Just ["NOT-A-VALID-HASH","FC8A15A2FAF2734DBB1DC5F7AFDC5C9BEAEB1F59"]
-- TORRENT ("FC8A15A2FAF2734DBB1DC5F7AFDC5C9BEAEB1F59"): "Ubuntu 15.04 Vivid Vervet Desktop amd64 ISO FINAL"
torrentGetList :: TransmissionConnection -> TorrentList -> TorrentFieldGetter a -> IO [a]
torrentGetList _   (Just []) _ = return []
torrentGetList con hashes (TorrentGetCons m) =
  let (PendingAction a, s) = runState m TorrentGet' { fieldRequestsTorrentGet' = TorrentFieldsRequest [] }
      sl = fieldRequestsTorrentGet' s
  in do resp <- sendRequest con (const . const 600) $ TorrentGet hashes sl
        case responseError resp of
         Just er -> fail $ "torrentGetList: " ++ T.unpack er
         Nothing -> case responseValue resp of
                     Nothing -> fail "torrentGetList: No response object."
                     Just rr -> return $ map a $ torrentInfoToList rr

-- | Applicative version of 'lookupTorrentField'. Requests a field for use.
getField :: Responsable t => TorrentField t -> TorrentFieldGetter t
getField f = TorrentGetCons $ do
  s <- get
  put $ s { fieldRequestsTorrentGet' = fieldRequestsTorrentGet' s +|+ f }
  return $ PendingAction $
    \z -> fromMaybe
          (error $ "getField: Field " ++ T.unpack (torrentFieldName f) ++ " is not in list.")
          (lookupTorrentField f z)


