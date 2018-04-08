{-# LANGUAGE OverloadedStrings #-}

module Main where

import Args (Options(..), getArgs)
import Search (search)
import Feed (toAtomFeed)
import Data.Time.LocalTime (getCurrentTimeZone)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  opts <- getArgs
  results_or_error <- search $ query opts
  case results_or_error of
    Left error -> print error
    Right results -> do
      tz <- getCurrentTimeZone
      TIO.putStrLn $ toAtomFeed tz (query opts) results
