module PlaylistConvert.Service
  (
    Track ( .. )
  , Id ( .. )
  , Service ( .. )
  ) where

import qualified Data.Text as T

data Id a = Id a T.Text deriving Show

data Track a = Track {
                       trackTitle :: T.Text
                     , trackAlbum :: T.Text
                     , trackArtists :: [T.Text]
                     , trackId :: Id a
                     } deriving Show

class Service a where
  -- | Search given 'Service' for tracks given a 'T.Text' query
  search :: a -> T.Text -> [Track a]

