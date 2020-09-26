{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module PlaylistConvert.Service.Tidal
  ( Tidal(..)
  )
where

import           PlaylistConvert.Service

import qualified Data.Text                     as T

import           Network.HTTP.Req
import           Data.Aeson
import           Data.Aeson.Types               ( Parser
                                                , parseMaybe
                                                , parseEither
                                                )

import           Data.Maybe
import Control.Applicative

data Tidal = Tidal deriving Show

tidalAPIUrl :: Url 'Https
tidalAPIUrl = https "listen.tidal.com" /: "v1"

tidalOpts :: Option 'Https
tidalOpts =
  "countryCode"
    =: ("US" :: String)
    <> "locale"
    =: ("en_US" :: String)
    <> "deviceType"
    =: ("BROWSER" :: String)
    <> header "x-tidal-token" "CzET4vdadNUFQ5JU"
    <> header "User-Agent" "Mozilla/5.0 (X11; Linux x86_64; rv:68.0) Gecko/20100101 Firefox/68.0"

paginate :: Page -> MaxItemsPerRequest -> Option 'Https
paginate p m = "offset" =: p <> "limit" =: m

parseTracks :: Value -> Parser [Track Tidal]
parseTracks = withObject "Tracks"
  $ \o -> searchStyle o <|> playlistStyle o
  where
    -- Case for 'playlist' requests
    playlistStyle o = mapM parseTrack =<< mapM (.: "item") =<< (o .: "items")
    -- Case for 'search' requests
    searchStyle o = mapM parseTrack =<< (.: "items") =<< (o .: "tracks")
    
-- TODO: sift out videos so it doesn't break when a playlist with them is encountered
parseTrack :: Object -> Parser (Track Tidal)
parseTrack o = Track <$> title <*> album <*> artists <*> tid
 where
  tid     = Id Tidal . T.pack . show <$> (o .: "id" :: Parser Int)
  title   = o .: "title"
  artists = mapM (.: "name") =<< (o .: "artists")
  album   = (.: "title") =<< (o .: "album")

instance Service Tidal where
  playlist (Id _ playlistId) = do
    response <- responseBody <$> req GET url NoReqBody jsonResponse opts
    return $ fromMaybe [] $ parseMaybe parseTracks response
   where
    url  = tidalAPIUrl /: "playlists" /: playlistId /: "items"
    opts = tidalOpts <> paginate 0 40

  search Tidal s = do
    response <- responseBody <$> req GET url NoReqBody jsonResponse opts
    return $ fromMaybe [] $ parseMaybe parseTracks response
   where
     url = tidalAPIUrl /: "search"
     opts = tidalOpts <> paginate 0 40 <> "query" =: s
       <> "types" =: ("TRACKS"::T.Text) <> "includeContributors" =: False