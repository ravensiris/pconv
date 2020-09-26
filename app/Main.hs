{-# LANGUAGE OverloadedStrings #-}
module Main where

import PlaylistConvert.Service
import PlaylistConvert.Service.Deezer
import PlaylistConvert.Service.Tidal

import Text.Show.Unicode

import Network.HTTP.Req

import Control.Monad.IO.Class

import Data.List

import Data.Maybe

main :: IO ()
main = runReq defaultHttpConfig $ do
  -- TODO: Fix paginated requests.
  -- They currently return only the first page
  -- They should load all the items
  -- Also add a paginated version for 'playlist' and 'search' after
  deezerToken <- deezerNotAuthenticatedToken
  playlistResult <- playlist $ Id Tidal "e95b130d-a6bc-4fa6-ab1d-0a2b5838d294"
--  c <- convert (Deezer deezerToken) (head playlistResult)
  playlistInDeezer <- mapM (convert (Deezer deezerToken)) (playlistResult)
  liftIO $ print $ map (trackTitle . fromJust) playlistInDeezer
  liftIO $ print $ map trackTitle playlistResult