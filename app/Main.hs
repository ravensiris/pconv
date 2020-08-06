{-# LANGUAGE OverloadedStrings #-}
module Main where

import PlaylistConvert.Service
import PlaylistConvert.Service.Deezer

import Text.Show.Unicode

import Network.HTTP.Req

import Control.Monad.IO.Class

main :: IO ()
main = runReq defaultHttpConfig $ do
  sq <- search (Deezer Nothing) "Get Lucky"
  liftIO $ putStrLn $ ushow sq
