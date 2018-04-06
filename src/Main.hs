{-# LANGUAGE OverloadedStrings #-}

module Main where

import Args (Options(..), getArgs)

main :: IO ()
main = do
  opts <- getArgs
  putStrLn $ show $ query opts
