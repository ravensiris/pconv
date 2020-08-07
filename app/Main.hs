{-# LANGUAGE OverloadedStrings #-}
module Main where

import PlaylistConvert.Service
import PlaylistConvert.Service.Deezer
import PlaylistConvert.Service.Tidal

import Text.Show.Unicode

import Network.HTTP.Req

import Control.Monad.IO.Class

import Data.List

main :: IO ()
main = runReq defaultHttpConfig $ do
  -- TODO: Fix paginated requests.
  -- They currently return only the first page
  -- They should load all the items
  -- Also add a paginated version for 'playlist' and 'search' after
  deezerToken <- deezerNotAuthenticatedToken
--  sq <- search (Deezer Nothing) "Get Lucky"
--  pl <- playlist $ Id (Deezer Nothing) "5354926046"
  -- searchResult <- search (Deezer deezerToken) "Get Lucky"
  -- playlistResult <- playlist $ Id (Deezer deezerToken) "5354926046"
  -- liftIO $ putStrLn $ intercalate "\n" $ map ushow $ playlistResult ++ searchResult
  playlistResult <- playlist $ Id Tidal "e95b130d-a6bc-4fa6-ab1d-0a2b5838d294"
  liftIO $ putStrLn $ intercalate "\n" $ map ushow $ playlistResult
  
