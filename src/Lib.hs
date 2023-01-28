module Lib
  ( run
  ) where

import           Control.Monad            (when)
import           Data.List                (intercalate, isPrefixOf)
import           Data.List.Split          (splitOn)
import           Data.Maybe               (fromMaybe)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Data.Time.LocalTime      (ZonedTime)
import           System.Environment       (lookupEnv)
import           System.Exit              (exitFailure)

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

toRecord :: String -> Commit -> IO String
toRecord repository (Commit _ an ae ad cn ce cd s) = do
  ztad <- iso8601ParseM ad :: IO ZonedTime
  ztcd <- iso8601ParseM cd :: IO ZonedTime
  -- "2023-02-02 13:10:49 +0900" => "2023-02-02 13:10:49"
  let sprittedZtad = " " `splitOn` show ztad
  let sprittedZtcd = " " `splitOn` show ztcd
  let at = head sprittedZtad ++ " " ++ sprittedZtad !! 1
  let ct = head sprittedZtcd ++ " " ++ sprittedZtcd !! 1
  return $
    intercalate
      "\t"
      [ repository
      , an
      , ae
      , at
      , cn
      , ce
      , ct
      , show ("Merge-pull-request-" `isPrefixOf` s)
      , s
      ]

run :: IO ()
run = do
  mRepository <- lookupEnv "REPOSITORY"
  let repository = fromMaybe "" mRepository
  contents <- getContents
  let commits = read $ "[" ++ intercalate "," (lines contents) ++ "]"
  null commits `when` exitFailure
  records <- mapM (toRecord repository) commits
  putStr $ unlines records
