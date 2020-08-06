{-# LANGUAGE OverloadedStrings #-}
module Main where

import PlaylistConvert.Service
import PlaylistConvert.Service.Deezer

import Text.Show.Unicode

import Network.HTTP.Req

import Control.Monad.IO.Class

import Data.List

main :: IO ()
main = runReq defaultHttpConfig $ do
  deezerToken <- deezerNotAuthenticatedToken
--  sq <- search (Deezer Nothing) "Get Lucky"
--  pl <- playlist $ Id (Deezer Nothing) "5354926046"
  searchResult <- search (Deezer deezerToken) "Get Lucky"
  playlistResult <- playlist $ Id (Deezer deezerToken) "5354926046"
  liftIO $ putStrLn $ intercalate "\n" $ map ushow $ playlistResult ++ searchResult
