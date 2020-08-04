{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Deezer (deezerSearch) where

import           PlaylistConvert.Internal

import qualified Data.Text                     as T
import qualified Data.ByteString               as B

import           Network.HTTP.Req
import           Network.HTTP.Client            ( CookieJar
                                                , Cookie(..)
                                                , insertCheckedCookie
                                                )
import           Network.HTTP.Client.Internal   ( expose )

import           Data.Aeson
import           Data.Aeson.Types               ( Parser
                                                , parseMaybe
                                                )
import           GHC.Generics                   ( Generic )

import           Data.Maybe                     ( fromJust )



data DeezerToken = DeezerToken String CookieJar deriving Show
instance ServiceToken DeezerToken where
  requestTokenOpts (DeezerToken token cookies) =
    "input"
      =: (3 :: Int)
      <> "api_version"
      =: ("1.0" :: String)
      <> "api_token"
      =: token
      <> cookieJar cookies


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


data Deezer = GetUserData | Search Page T.Text

deezerMaxItemsPerPage :: Int
deezerMaxItemsPerPage = 40

deezer GetUserData = req GET deezerAPIUrl NoReqBody jsonResponse opts
 where
  opts =
    requestTokenOpts (DeezerToken "" mempty)
      <> "method"
      =: ("deezer.getUserData" :: String)
deezer (Search p s) = do
  opts <-
    ("method" =: ("search.music" :: String) <>)
    .   requestTokenOpts
    .   fromJust
    <$> deezerToken ""
  req POST deezerAPIUrl jsonQuery jsonResponse opts
 where
  jsonQuery = ReqBodyJson $ DeezerQuery { query         = s
                                        , start = p * deezerMaxItemsPerPage
                                        , nb            = deezerMaxItemsPerPage
                                        , Deezer.filter = "all"
                                        , output        = "TRACK"
                                        }

type DeezerARL = B.ByteString

-- | Create a 'Token' based on 'DeezerARL', leave an empty string in place of
--   'DeezerARL' if you don't need access to user data
deezerToken :: (MonadHttp m) => DeezerARL -> m (Maybe DeezerToken)
deezerToken arl = do
  response <- deezer GetUserData
  let token         = parseMaybe parseDeezerToken $ responseBody response
      cookies       = responseCookieJar response
      anyCookie     = head $ expose cookies
      anyAccessTime = cookie_last_access_time anyCookie
      anyExpiry     = cookie_expiry_time anyCookie
      anyCreation   = cookie_creation_time anyCookie
      arlCookie'    = arlCookie anyExpiry anyCreation anyAccessTime
      cookies'      = insertCheckedCookie arlCookie' cookies True
  return $ case token of
    Just t  -> Just $ DeezerToken t cookies'
    Nothing -> Nothing
 where
  parseDeezerToken :: Value -> Parser String
  parseDeezerToken =
    withObject "DeezerToken" $ \o -> (.: "checkForm") =<< (o .: "results")
  arlCookie expiry creation access = Cookie { cookie_name             = "arl"
                                            , cookie_value            = arl
                                            , cookie_expiry_time      = expiry
                                            , cookie_domain = "deezer.com"
                                            , cookie_path             = "/"
                                            , cookie_creation_time    = creation
                                            , cookie_last_access_time = access
                                            , cookie_persistent       = True
                                            , cookie_host_only        = False
                                            , cookie_secure_only      = True
                                            , cookie_http_only        = True
                                            }


deezerSearch :: MonadHttp m => Page -> T.Text -> m (Maybe [Track])
deezerSearch p s = do
  response <- deezer $ Search p s
  return $ parseMaybe parseTracks $ responseBody response
 where
  parseTracks :: Value -> Parser [Track]
  parseTracks = withObject "Tracks" $ \o -> (.: "data") =<< (o .: "results")
