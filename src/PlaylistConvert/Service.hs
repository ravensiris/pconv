{-# LANGUAGE OverloadedStrings #-}
module PlaylistConvert.Service
  (
    Track ( .. )
  , Id ( .. )
  , Service ( .. )
  , Page
  , MaxItemsPerRequest
  , convert
  ) where

import qualified Data.Text as T

import           Network.HTTP.Req ( MonadHttp )
import Data.Maybe (listToMaybe)
-- | For paginated API requests
type Page = Int

-- | Item request limit
type MaxItemsPerRequest = Int

data Id a = Id a T.Text deriving Show

data Track a = Track {
                       trackTitle :: T.Text
                     , trackAlbum :: T.Text
                     , trackArtists :: [T.Text]
                     , trackId :: Id a
                     } deriving Show

class Service a where
  -- | Search given 'Service' for tracks given a 'T.Text' query
  search :: (MonadHttp m) => a -> T.Text -> m [Track a]
  -- | Return a list of 'Track' given a playlist id
  playlist :: (MonadHttp m) => Id a -> m [Track a]


-- | Convert 'Track' between 'Service's
convert :: (MonadHttp m, Service a, Service b) => a -> Track b -> m (Maybe (Track a))
convert s Track {trackTitle=title, trackArtists=artists} = 
  listToMaybe <$> search s query
  where
    query = T.intercalate " " [(head artists), title]
