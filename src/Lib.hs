{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Codec.Binary.UTF8.String       ( encode )
import qualified Data.ByteString               as BS
import           Data.List                      ( find
                                                , isPrefixOf
                                                )
import           Data.List.Split                ( splitOn )
import qualified Data.Map.Strict               as Map
import           Data.UnixTime                  ( UnixTime(..)
                                                , mailDateFormat
                                                , parseUnixTime
                                                )
import           System.Environment             ( getArgs )

data Commit = Commit
  { commit  :: String
  , author  :: String
  , date    :: UnixTime
  , added   :: Int
  , removed :: Int
  }
  deriving (Eq, Show)

data Contribute = Contribute
  { commitCount :: Int
  , addCount    :: Int
  , removeCount :: Int
  }
  deriving (Eq, Show)

createDefaultCommit :: Commit
createDefaultCommit = Commit "" "" (UnixTime 0 0) 0 0

trim :: String -> String
trim = go . reverse . go . reverse
 where
  go :: String -> String
  go (' ' : cs) = go cs
  go s          = s

getValue :: String -> String -> String
getValue line sep = trim (splitOn sep line !! 1)

gitFormat :: BS.ByteString
gitFormat = "%a %b %d %H:%M:%S %Y %z"

iso8601Format :: BS.ByteString
iso8601Format = "%Y-%m-%d"

pack :: String -> BS.ByteString
pack = BS.pack . encode

parseLogs :: String -> UnixTime -> UnixTime -> [String] -> [Commit]
parseLogs mode from to = go createDefaultCommit []
 where
  go :: Commit -> [Commit] -> [String] -> [Commit]
  go commit acc [] = reverse $ commit : acc
  go commit acc list@(line : lines)
    | "commit " `isPrefixOf` line
    = if commit == createDefaultCommit && mode == "commit"
      then go commit { commit = splitOn " " line !! 1 } acc lines
      else go createDefaultCommit (commit : acc) list
    | "Author: " `isPrefixOf` line
    = go commit { author = splitOn ": " line !! 1 } acc lines
    | "Date: " `isPrefixOf` line
    = let date = parseUnixTime gitFormat $ pack $ getValue line ": "
      in  case (date >= from, date <= to) of
            (True, True) -> go
              commit
                { date = parseUnixTime gitFormat $ pack $ getValue line ": "
                }
              acc
              lines
            (_, False) -> go commit acc lines
            _          -> go commit acc []
    | "+" `isPrefixOf` line
    = if mode == "code"
      then go commit { added = added commit + 1 } acc lines
      else go commit acc lines
    | "-" `isPrefixOf` line
    = if mode == "code"
      then go commit { removed = removed commit + 1 } acc lines
      else go commit acc lines
    | otherwise
    = go commit acc lines

convert :: [Commit] -> Map.Map String Contribute
convert = go Map.empty
 where
  go :: Map.Map String Contribute -> [Commit] -> Map.Map String Contribute
  go acc [] = acc
  go acc (commit : commits) =
    let a     = author commit
        found = Map.lookup a acc
    in  case found of
          Nothing -> go
            (Map.insert a (Contribute 1 (added commit) (removed commit)) acc)
            commits
          (Just contribute) -> go
            (Map.update
              (\contribute -> Just contribute
                { commitCount = commitCount contribute + 1
                , addCount    = addCount contribute + added commit
                , removeCount = removeCount contribute + removed commit
                }
              )
              a
              acc
            )
            commits

calcValues :: String -> Contribute -> Double
calcValues mode contribute = case mode of
  "commit" -> cc
  "code"   -> ac * 10 + rc * (1 + cc / 100)
  _        -> 0
 where
  cc = fromIntegral $ commitCount contribute
  ac = fromIntegral $ addCount contribute
  rc = fromIntegral $ removeCount contribute

searchArgs :: String -> String -> [String] -> String
searchArgs word defaultValue args =
  let found = find (word `isPrefixOf`) args
  in  case found of
        Nothing  -> defaultValue
        (Just s) -> drop (length word) s

main' :: IO ()
main' = do
  args     <- getArgs
  contents <- getContents
  let mode       = searchArgs "mode=" "commit" args
  let from       = searchArgs "from=" "1970-01-01" args
  let to         = searchArgs "to=" "2038-01-19" args
  let from'      = parseUnixTime iso8601Format $ pack from
  let to'        = parseUnixTime iso8601Format $ pack to
  let allCommits = parseLogs mode from' to' $ lines contents
  let commits = filter
        (\commit -> date commit >= from' && date commit <= to')
        allCommits
  let committers     = convert commits
  let listCommitters = Map.toList committers
  let labels = map (\(label, _) -> head (splitOn " <" label)) listCommitters
  let values         = map (calcValues mode . snd) listCommitters
  putStrLn "cat python/bar.py | \\"
  putStrLn $ "sed 's/labels = \\[\\]/labels = " ++ show labels ++ "/' | \\"
  putStrLn $ "sed 's/values = \\[\\]/values = " ++ show values ++ "/'"
