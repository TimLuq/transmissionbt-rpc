{-# LANGUAGE OverloadedStrings #-}
--module Network.BitTorrent.Transmission.Test where
module Main where

import Prelude as P
import System.Environment (lookupEnv)
import Data.List as P
import Data.HashMap.Strict (foldrWithKey)
import Network.BitTorrent.Transmission
import Network.BitTorrent.Transmission.Internal (runBrowserActionUnsafe)
import Network.URI (parseURI)
import qualified Network.Browser as Browser
import Data.Foldable
import Data.Char
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import qualified Data.Aeson as JSON

serverRPC :: IO String
serverRPC = lookupEnv "TRANSMISSIONBT_TEST_URL" >>= \x -> return $ case x of
  Nothing -> "http://localhost:9091/transmission/rpc"
  Just x' -> x'

main :: IO ()
main = do
  (Just service) <- fmap parseURI serverRPC
  tcon <- transmissionConnect service
  runBrowserActionUnsafe tcon $ Browser.setDebugLog $ Just "/tmp/hs-transmissionbt-rpc.log"
  useractions tcon

flush :: IO ()
flush = hFlush stdout

list :: TorrentFieldsRequest -> TransmissionConnection -> IO ()
list tfr tcon = do
  resp <- sendRequest tcon (\_ _ -> 1) $ TorrentGet allTorrents tfr
  details' $ responseValue resp

details' :: Maybe TorrentInfoList -> IO ()
details' (Nothing) = putStrLn "Something went wrong..."
details' (Just rs) = forM_ (torrentInfoToList rs) $ details''

details'' :: (TorrentFieldsContainer t) => t -> IO ()
details'' x' = do
  let x = torrentFields x'
  putStr ">> "
  case lookupTorrentField TorrentHashString x of
   Nothing -> putStr "                                        "
   Just xh -> T.putStr xh
  putStr "  "
  case lookupTorrentField TorrentName x of
   Nothing -> putStrLn "(no name)"
   Just xn -> putStrLn $ show xn
   
  forM_ (torrentFieldsToList x) $ \(a, b) -> do
    putStr "  "
    T.putStr a
    putStr ": "
    putStr $ replicate (24 - T.length a) ' '
    putStrLn $ showjson b
  where
    showjson j = case j of
      JSON.Null     -> "null"
      JSON.String y -> show y
      JSON.Number y -> show y
      JSON.Bool   y -> if y then "true" else "false"
      JSON.Array  y -> '[' : P.intercalate "," (map showjson $ toList y) ++ "]"
      JSON.Object y -> '{' : P.intercalate "," (foldrWithKey (\k a b -> ((show k) ++ ": " ++ showjson a) : b) [] y) ++ "}"
      y             -> show y

details_ :: TransmissionConnection -> IO ()
details_ con = do
  d <- read <$> fromMaybe "140" <$> lookupEnv "COLUMNS"
  r <- torrentGetList con allTorrents $
       f d <$> getField TorrentHashString <*> getField TorrentName
  putStrLn $ "Result size: " ++ show (length r)
  P.mapM_ putStrLn r
  where f d hash name =
          let maxl  = if d < 80 then 20 else d - 60
              name' = take maxl $ init $ tail $ show name
          in ">> " ++ T.unpack hash ++ "  " ++ name' ++ replicate (maxl + 2 - length name') ' '
  

details :: T.Text -> TransmissionConnection -> IO ()
details hash tcon = do
  resp <- sendRequest tcon (\_ _ -> 1) $ TorrentGet (Just [hash]) allTorrentFields
  let rv = responseValue resp
  case rv of { Nothing -> return () ; Just rs -> if null $ torrentInfoToList rs then putStrLn "No matching torrent." else return () }
  details' rv

fileDetails :: T.Text -> TransmissionConnection -> IO ()
fileDetails hash tcon = do
  resp <- sendRequest tcon (\_ _ -> 4) $ TorrentGet (Just [hash]) $ makeFieldsRequest TorrentHashString +|+ TorrentName +|+ TorrentFiles +|+ TorrentFileStats
  case responseValue resp of
   Nothing -> putStrLn "Parsing error."
   Just rv -> case torrentInfoToList rv of
     [] -> putStrLn "No matching torrent."
     rs -> forM_ rs $ \r -> do
       putStr ">> "
       case lookupTorrentField TorrentHashString r of
        Nothing -> putStr "                                        "
        Just xh -> T.putStr xh
       putStr "  "
       case lookupTorrentField TorrentName r of
        Nothing -> putStrLn "(no name)"
        Just xn -> putStrLn $ show xn
       let (Just fds) = lookupTorrentField TorrentFiles r
           (Just fss) = lookupTorrentField TorrentFileStats r
       forM_ (zip (asList fds) (asList fss)) $ \(fd, fs) -> do
         putStr $ if fileWanted fs then "  X " else "    "
         putStr $ if fileLength fd == fileCompleted fs then "DONE "
                  else let x  = show ((floor $ (100.0 :: Double) * (fromIntegral (fileCompleted fs) / fromIntegral (fileLength fd))) :: Int)
                           xl = length x
                       in (if xl == 3 then x else replicate (3 - xl) ' ' ++ x) ++ "% "
         putStrLn $ show $ fileName fd

add :: T.Text -> TransmissionConnection -> IO ()
add ident tcon =
  let uri = if T.all isHexDigit ident && T.length ident == 40 then "magnet:?xt=urn:btih:" `T.append` T.map toUpper ident
            else if "magnet:" `T.isPrefixOf` ident || "http://" `T.isPrefixOf` ident || "https://" `T.isPrefixOf` ident then ident
                 else error $ "add: unknown fetch type " ++ show ident
  in do resp <- sendRequest tcon (\_ _ -> 2) $ TorrentAdd uri unspecifiedDirectory Nothing
        case responseError resp of
         Just err -> error $ "add: error " ++ show err
         Nothing  -> case responseValue resp of
           Nothing -> error "add: no response value"
           Just rv -> case rv of
             Right rv' -> putStrLn "Added a new torrent." >> details'' rv'
             Left  rv' -> putStrLn "Torrent have been previously added." >> details'' rv'
             

useractions :: TransmissionConnection -> IO ()
useractions tcon = do
  putStrLn ""
  listActions
  putStrLn "q) Quit"
  putStrLn ""
  putStr "Option: "
  flush
  x <- getLine
  if (not . null) x && P.all isDigit x
    then let i = read x
         in if i >= length actions then putStrLn "-- invalid option." >> putStrLn "" >> useractions tcon
            else (snd (actions !! i)) tcon >> useractions tcon
    else putStrLn "Quitting..."
  where actions = [ ("List all torrents",   list (makeFieldsRequest TorrentHashString +|+ TorrentName))
                  , ("Detailed listing",    list allTorrentFields)
                  , ("Applicative listing", details_)
                  , ("Torrent details",     (\c -> (putStr "Torrent hash: " >> flush >> T.getLine >>= \h -> details h c)))
                  , ("Torrent files",       (\c -> (putStr "Torrent hash: " >> flush >> T.getLine >>= \h -> fileDetails h c)))
                  , ("Add torrent (by hash, magnet, or URL)", (\c -> (putStr "Torrent identifier: " >> flush >> T.getLine >>= \h -> add h c)))
                  ]
        listActions = listActions' 0 actions
        listActions' :: Int -> [(String, (TransmissionConnection -> IO ()))] -> IO ()
        listActions' _ [] = return ()
        listActions' i ((x, _):xs) = putStrLn (show i ++ ") " ++ x) >> listActions' (i+1) xs
