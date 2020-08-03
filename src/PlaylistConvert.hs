{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module PlaylistConvert where

import qualified Data.Text as T
import qualified Data.ByteString as B

import Network.HTTP.Req
import Network.HTTP.Client ( CookieJar , Cookie (..) , insertCheckedCookie )
import Network.HTTP.Client.Internal (expose)

import Data.Aeson
import Data.Aeson.Types ( Parser, parseMaybe )
import GHC.Generics ( Generic )

import Control.Monad.IO.Class
import Data.Maybe
import Text.Show.Unicode ( ushow )

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
  parseJSON = withObject "Track" $ parseDeezerTrack
  
class ServiceToken a where
-- | Create 'Option' with 'ServiceToken' embedded inside
  requestTokenOpts :: a -> Option 'Https

newtype TidalToken = TidalToken B.ByteString deriving Show
instance ServiceToken TidalToken where
    requestTokenOpts (TidalToken token) = "countryCode" =: ("US"::String)
      <> oAuth2Bearer token
data DeezerToken = DeezerToken String CookieJar deriving Show
instance ServiceToken DeezerToken where
    requestTokenOpts (DeezerToken token cookies) =
        "input" =: (3::Int)
        <> "api_version" =: ("1.0" :: String)
        <> "api_token" =: token
        <> cookieJar cookies

-- | Base 'Url' for all Tidal API operations
tidalAPIUrl :: Url 'Https
tidalAPIUrl = https "listen.tidal.com" /: "v1"

-- | Base 'Url' for all Deezer API operations
deezerAPIUrl :: Url 'Https
deezerAPIUrl = https "www.deezer.com" /: "ajax" /: "gw-light.php"


data DeezerQuery = DeezerQuery
  { query :: T.Text,
    start :: Int,
    nb :: Int,
    filter :: String,
    output :: String
  }
  deriving (Show, Generic)

instance ToJSON DeezerQuery

type Page = Int

data Deezer = GetUserData | Search DeezerToken Page T.Text

deezerMaxItemsPerPage :: Int
deezerMaxItemsPerPage = 40

deezer GetUserData = req GET deezerAPIUrl NoReqBody jsonResponse opts
  where
    opts = requestTokenOpts (DeezerToken "" mempty)
      <> "method" =: ("deezer.getUserData"::String)
deezer (Search t p s) = req POST deezerAPIUrl jsonQuery jsonResponse opts
  where
    opts = requestTokenOpts t
      <> "method" =: ("search.music"::String)
    jsonQuery =
      ReqBodyJson $
        DeezerQuery
          { query = s,
            start = p * deezerMaxItemsPerPage,
            nb = deezerMaxItemsPerPage,
            PlaylistConvert.filter = "all",
            output = "TRACK"
          }

type DeezerARL = B.ByteString

-- | Create a 'Token' based on 'DeezerARL', leave an empty string in place of
--   'DeezerARL' if you don't need access to user data
deezerToken :: (MonadHttp m) => DeezerARL -> m (Maybe DeezerToken)
deezerToken arl = do
  response <- deezer GetUserData
  let token = parseMaybe parseDeezerToken $ responseBody response
      cookies = responseCookieJar response
      anyCookie = head $ expose cookies
      anyAccessTime = cookie_last_access_time anyCookie
      anyExpiry = cookie_expiry_time anyCookie
      anyCreation = cookie_creation_time anyCookie
      arlCookie' = arlCookie anyExpiry anyCreation anyAccessTime
      cookies' = insertCheckedCookie arlCookie' cookies True
  return $ case token of
    Just t -> Just $ DeezerToken t cookies'
    Nothing -> Nothing
  where
    parseDeezerToken :: Value -> Parser String
    parseDeezerToken = withObject "DeezerToken" $ \o -> (.: "checkForm") =<< (o .: "results")
    arlCookie expiry creation access =
      Cookie
        { cookie_name = "arl",
          cookie_value = arl,
          cookie_expiry_time = expiry,
          cookie_domain = "deezer.com",
          cookie_path = "/",
          cookie_creation_time = creation,
          cookie_last_access_time = access,
          cookie_persistent = True,
          cookie_host_only = False,
          cookie_secure_only = True,
          cookie_http_only = True
        }


deezerSearch :: MonadHttp m => DeezerToken -> Page -> T.Text -> m (Maybe [Track])
deezerSearch t p s = do
  response <- deezer $ Search t p s
  return $ parseMaybe parseTracks $ responseBody response
  where
    parseTracks :: Value -> Parser [Track]
    parseTracks = withObject "Tracks" $ \o -> (.: "data") =<< (o .: "results")

main :: IO ()        
main = runReq defaultHttpConfig $ do
  token <- deezerToken ""
  sr <- deezerSearch (fromJust token) 0 "Get Lucky"
  liftIO $ putStrLn $ ushow sr

