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

-- Extracted from app.VERSION_NUMBER.chunk.js
--
-- Looking at the code it seems it is profiling devices and assigning tokens based on device/carrier
-- u = o.a.USE_STAGE_APIS
-- ? 'YuBokFUKeupw1zTdlyw4' : Object(a.isTizen) ()
-- ? 'M6ztoSvmny6alVCD' : Object(a.isVizio) ()
-- ? 'Y40WSvVnnG0ql0L0' : Object(a.isVodafoneSTB) ()
-- || Object(a.isTV) ()
-- ? 'NIh99tUmaAyLNmEA' : Object(a.isWindowsStore) ()
-- ? 'jdDuod31BUA6qXXq' : l
-- ? 'qe5mgUGPtIfbgN574ngS74Sd1OmKIfvcLx7e28Yk' : 'gsFXkJqGrUNoYMQPZe4k3WKwijnrp8iGSwn3bApe'
--
--       d = o.a.USE_STAGE_APIS
-- ? 'YuBokFUKeupw1zTdlyw4' : Object(a.isTizen) ()
-- ? 'M6ztoSvmny6alVCD' : Object(a.isVizio) ()
-- ? 'Y40WSvVnnG0ql0L0' : Object(a.isVodafoneSTB) ()
-- || Object(a.isTV) ()
-- ? 'NIh99tUmaAyLNmEA' : Object(a.isWindowsStore) ()
--    ^ yes
-- ? 'VGGyfsDBQnKqz0W3' : l
--    ^ yes
-- ? 'u5qPNNYIbD0S0o36MrAiFZ56K6qMCrCmYPzZuTnV' : 'y3Ab6MUg5bjjofvu'
--    ^ yes                                        ^ yes
--
--       b = o.a.USE_STAGE_APIS
-- ? 'YuBokFUKeupw1zTdlyw4' : Object(a.isTizen) ()
--    ^ no
-- ? 'M6ztoSvmny6alVCD' : Object(a.isVizio) ()
--    ^ yes
-- ? 'Y40WSvVnnG0ql0L0' : Object(a.isVodafoneSTB) ()
--    ^ yes
-- || Object(a.isTV) ()
-- ? 'NIh99tUmaAyLNmEA' : Object(a.isWindowsStore) ()
--    ^ yes
-- ? 'VGGyfsDBQnKqz0W3' : l
--    ^ yes
-- ? 'u5qPNNYIbD0S0o36MrAiFZ56K6qMCrCmYPzZuTnV' : 'CzET4vdadNUFQ5JU'
--    ^ yes                                        ^ yes
--
--       y = o.a.USE_STAGE_APIS
-- ? 'YuBokFUKeupw1zTdlyw4' : 'DfPdNV5M8ZYTLwqW2sZfPFm1ce6erS';
--    ^ no                     ^ yes

--xTidalHeaders = 

tidalOpts :: Option 'Https
tidalOpts =
  "countryCode"
    =: ("US" :: String)
    <> "locale"
    =: ("en_US" :: String)
    <> "deviceType"
    =: ("BROWSER" :: String)
    <> header "x-tidal-token" "CzET4vdadNUFQ5JU"

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
-- TODO: sift out videos so it doesn't break when a playlist is encountered
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
