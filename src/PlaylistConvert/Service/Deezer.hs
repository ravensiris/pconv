{-# LANGUAGE OverloadedStrings #-}
module PlaylistConvert.Service.Deezer
  (
    Deezer ( .. )
  ) where

import PlaylistConvert.Service

data Deezer = Deezer deriving Show

instance Service Deezer where
  search _ _ = []

main :: IO ()
main = print $ search Deezer ""
