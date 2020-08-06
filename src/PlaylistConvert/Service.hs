module PlaylistConvert.Service
  (
    Track ( .. )
  , Id ( .. )
  , Service ( .. )
  , Page
  , MaxItemsPerRequest 
  ) where

import qualified Data.Text as T

import           Network.HTTP.Req ( MonadHttp )

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

