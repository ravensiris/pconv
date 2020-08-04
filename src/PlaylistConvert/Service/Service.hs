module PlaylistConvert.Service where

import qualified Data.Text as T

type Page = Int

newtype Match a = Match a


data Track = Track { trackTitle :: T.Text
                   , trackAlbum :: T.Text
                   , trackArtists :: [T.Text]
                   , trackId :: Id
                   } deriving Show

class Service a where
  paginatedSearchTracks :: a -> Page -> Maybe (Match [Track])


