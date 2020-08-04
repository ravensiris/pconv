{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module PlaylistConvert.Internal
  ( Id(..)
  , Track(..)
  , Page
  , ServiceToken(..)
  )
where

import qualified Data.Text                     as T
import qualified Data.ByteString               as B

import           Network.HTTP.Req

import           Data.Aeson
import           Data.Aeson.Types               ( Parser )


data Id = TidalId T.Text | DeezerId T.Text deriving Show

data Track = Track { trackTitle :: T.Text
                   , trackAlbum :: T.Text
                   , trackArtists :: [T.Text]
                   , trackId :: Id
                   } deriving Show


-- | Parsing 'Track' from Deezer JSON 'Object's
parseDeezerTrack :: Object -> Parser Track
parseDeezerTrack o = Track <$> title <*> album <*> artists <*> tid
 where
  tid     = DeezerId <$> o .: "SNG_ID"
  title   = o .: "SNG_TITLE"
  artists = mapM (.: "ART_NAME") =<< o .: "ARTISTS"
  album   = o .: "ALB_TITLE"

instance FromJSON Track where
  parseJSON = withObject "Track" parseDeezerTrack

class ServiceToken a where
-- | Create 'Option' with 'ServiceToken' embedded inside
  requestTokenOpts :: a -> Option 'Https

-- | Page, used in paginated requests such as searching for 'Track's
type Page = Int
