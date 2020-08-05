{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module PlaylistConvert.Service where

import qualified Data.Text as T

data family Service a

instance Show (Service a)

data Deezer = DeezerService deriving Show
data Tidal = TidalService deriving Show

data instance Service Deezer = Deezer 
data instance Service Tidal = Tidal

instance Action (Service Deezer) where
  search a b = [dummyTrack (Deezer)]
instance Action (Service Tidal) where
  search a b = [dummyTrack (Tidal)]

class Action a where
  search :: a -> T.Text -> [Track a]

data Id a = Id a T.Text deriving Show

data Track a = Track { trackTitle :: T.Text
                   , trackAlbum :: T.Text
                   , trackArtists :: [T.Text]
                   , trackId :: a
                   } deriving Show

dummyTrack s = Track s "Title" "Album" ["Artist A", "Artist B"]

main :: IO ()
main = print $ search Tidal ""

