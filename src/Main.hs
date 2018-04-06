{-# LANGUAGE OverloadedStrings #-}

module Main where

import Args (Options(..), getArgs)
import Search (search)

main :: IO ()
main = do
  opts <- getArgs
  results <- search $ query opts
  print results
