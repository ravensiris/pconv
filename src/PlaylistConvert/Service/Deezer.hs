{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PlaylistConvert.Service.Deezer
  ( Deezer(..)
  , deezerNotAuthenticatedToken
  , playlist
  )
where

import           PlaylistConvert.Service

import qualified Data.Text as T

import           Data.Aeson
import           Data.Aeson.Types               ( Parser
                                                , parseMaybe
                                                , parseEither
                                                )
import           Network.HTTP.Client            ( CookieJar )


import GHC.Generics                 

import           Network.HTTP.Req

import Data.Maybe
import Control.Applicative
import Data.List


-- | Base 'Url' for all Deezer API operations
deezerAPIUrl :: Url 'Https
deezerAPIUrl = https "www.deezer.com" /: "ajax" /: "gw-light.php"


data DeezerToken = DeezerToken String CookieJar deriving Show

newtype Deezer = Deezer (Maybe DeezerToken) deriving Show

data DeezerQuery = DeezerQuery
  { query :: T.Text,
    start :: Page,
    nb :: MaxItemsPerRequest,
    filter :: String,
    output :: String
  }
  deriving (Show, Generic)

instance ToJSON DeezerQuery

deezerQuery :: T.Text -> Page -> MaxItemsPerRequest -> DeezerQuery
deezerQuery query page maxItems = DeezerQuery
        {
          query = query
        , start = page
        , nb = maxItems
        , PlaylistConvert.Service.Deezer.filter = "all"
        , output = "TRACK"
        }

-- | Parse a list of 'Track'        
parseTracks :: Value -> Parser [Track Deezer]
parseTracks = withObject "Tracks" $ \o -> mapM parseTrack =<< (.: "data") =<< (o .: "results")

-- | Parsing 'Track' from Deezer JSON 'Object's
parseTrack :: Object -> Parser (Track Deezer)
parseTrack o = Track <$> title <*> album <*> (artists <|> artist) <*> tid
    where
        tid     = Id (Deezer Nothing) <$> o .: "SNG_ID"
        title   = o .: "SNG_TITLE"
        artists = mapM (.: "ART_NAME") =<< o .: "ARTISTS"
        artist = (`insert` []) <$> (o .: "ART_NAME" :: Parser T.Text)
        album   = o .: "ALB_TITLE"


data DeezerMethod = GetUserData | MusicSearch | PlaylistGetSongs

requestOpts :: DeezerToken -> DeezerMethod -> Option 'Https
requestOpts (DeezerToken token cookies) method =
    "input"
        =: (3 :: Int)
        <> "api_version"
        =: ("1.0" :: String)
        <> "api_token"
        =: token
        <> cookieJar cookies
        <> "method"
        =: methodS method
     where
       methodS :: DeezerMethod -> String
       methodS GetUserData = "deezer.getUserData"
       methodS MusicSearch = "search.music"
       methodS PlaylistGetSongs = "playlist.getSongs"
    
deezerNotAuthenticatedToken :: (MonadHttp m) => m (Maybe DeezerToken)
deezerNotAuthenticatedToken  = do
  response <- req GET deezerAPIUrl NoReqBody jsonResponse opts
  let token = parseMaybe parseDeezerToken (responseBody response)
      cookies = responseCookieJar response
  return $ (\t -> Just $ DeezerToken t cookies) =<< token
  where
    opts = requestOpts (DeezerToken "" mempty) GetUserData
    parseDeezerToken :: Value -> Parser String
    parseDeezerToken =
        withObject "DeezerToken" $ \o -> (.: "checkForm") =<< (o .: "results")

deezerSearch :: (MonadHttp m) =>
  DeezerToken -> Page -> MaxItemsPerRequest -> T.Text -> m (Maybe [Track Deezer])
deezerSearch token page maxItems query = do
  response <- responseBody <$> req POST deezerAPIUrl jsonQuery jsonResponse opts
  return $ parseMaybe parseTracks response
  where
    opts = requestOpts token MusicSearch
    jsonQuery = ReqBodyJson $ deezerQuery query page maxItems



type DeezerPlaylistId = T.Text

data DeezerPlaylistQuery = DeezerPlaylistQuery
  {
    nb :: MaxItemsPerRequest
  , playlist_id :: DeezerPlaylistId
  , start :: Page
  } deriving (Show, Generic)
instance ToJSON DeezerPlaylistQuery

deezerPlaylist :: (MonadHttp m) =>
  DeezerToken -> Page -> MaxItemsPerRequest -> DeezerPlaylistId -> m (Maybe [Track Deezer])
deezerPlaylist token page maxItems playlistId = do
  response <- req POST deezerAPIUrl jsonQuery jsonResponse opts
  return $ parseMaybe parseTracks $ responseBody response
  where
    opts = requestOpts token PlaylistGetSongs
    jsonQuery = ReqBodyJson $ DeezerPlaylistQuery maxItems playlistId page
                
instance Service Deezer where
  -- | For multiple searches it is advised to create a token once
  --
  --   Take a look at 'deezerNotAuthenticatedToken'
  search (Deezer (Just token)) s = fromMaybe [] <$> deezerSearch token 0 40 s
  -- | With no 'DeezerToken' given it will create a unauthenticated token
  --
  --   Keep in mind that with multiple requests it will come with 
  --   an additional HTTP request per search
  --
  --   Take a look at 'deezerNotAuthenticatedToken'
  search (Deezer Nothing) s = do
    -- WARN: Might result in stack overflow
    -- TODO: Handle error when getting the token results in failure
    token <- Deezer <$> deezerNotAuthenticatedToken
    search token s

  playlist (Id (Deezer (Just token)) playlistId) = fromMaybe []
    <$> deezerPlaylist token 0 2000 playlistId
  playlist (Id (Deezer Nothing) playlistId)= do
    -- WARN: Might result in stack overflow
    -- TODO: Handle error when getting the token results in failure
    token <- Deezer <$> deezerNotAuthenticatedToken
    playlist $ Id token playlistId