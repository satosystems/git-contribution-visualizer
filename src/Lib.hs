module Lib
  ( run
  ) where

import           Data.List                (intercalate)
import           Data.Maybe               (fromJust, fromMaybe, isJust)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Data.Time.LocalTime      (ZonedTime)
import           System.Environment       (getArgs, lookupEnv)

data Commit =
  Commit
    { commitHash     :: String -- ^ commit hash: "e04605b44a65f55cbfb9fb6c59a08454915aaad4"
    , authorName     :: String -- ^ author name: "Satoshi Ogata"
    , authorEmail    :: String -- ^ author email: "satosystems@gmail.com"
    , authorDate     :: String -- ^ author date, strict ISO 8601 format: "2023-01-28T20:36:58+09:00"
    , committerName  :: String -- ^ committer name: "Satoshi Ogata"
    , committerEmail :: String -- ^ committer email: "satosystems@gmail.com"
    , committerDate  :: String -- ^ committer date, strict ISO 8601 format: "2023-01-29T08:51:10+09:00"
    , subject        :: String -- ^ subject: "Add-all-created-files-by-stack-new"
    }
  deriving (Show, Read)

data Input =
  Input
    { mergeCommits :: [String]
    , commits      :: [Commit]
    }
  deriving (Read, Show)

toRecord :: String -> [String] -> Commit -> String
toRecord repository mchs (Commit ch an ae ad cn ce cd s) =
  let ztad = fromJust $ iso8601ParseM ad :: ZonedTime
      ztcd = fromJust $ iso8601ParseM cd :: ZonedTime
      -- "2023-02-02 13:10:49 +0900" => "2023-02-02 13:10:49"
      f :: ZonedTime -> String
      f zt =
        let spritted = words $ show zt
         in head spritted ++ " " spritted !! 1
      at = f ztad
      ct = f ztcd
   in intercalate
        "\t"
        [repository, an, ae, at, cn, ce, ct, show (ch `elem` mchs), s]

listCommits :: String -> String -> String
listCommits repository contents =
  let Input mchs cs = read contents
      mchs' = tail mchs
      cs' = tail cs
   in unlines $ map (toRecord repository mchs') cs'

countBlames :: String -> String -> String
countBlames repository contents = go (lines contents) []
  where
    go :: [String] -> [(String, Int)] -> String
    go [] acc =
      foldl
        (\s (name, count) ->
           s ++ "\n" ++ repository ++ "\t" ++ name ++ "\t" ++ show count ++ "\n")
        ""
        acc
    go (s:ss) acc =
      let name = (unwords . reverse . drop 4 . reverse . words) s
          mCount = lookup name acc
       in if isJust mCount
            then let count = fromJust mCount
                  in go ss $
                     (name, succ count) :
                     filter (\(name', _) -> name /= name') acc
            else go ss $ (name, 1) : acc

run :: IO ()
run = do
  mRepository <- lookupEnv "REPOSITORY"
  let repository = fromMaybe "" mRepository
  contents <- getContents
  args <- getArgs
  let fn =
        if head args == "listCommits"
          then listCommits
          else countBlames
  putStr $ fn repository contents
