{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Tidal where

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

-- | Base 'Url' for all Tidal API operations
tidalAPIUrl :: Url 'Https
tidalAPIUrl = https "listen.tidal.com" /: "v1"

noTokenOpts = "countryCode" =: ("US" :: String)

newtype TidalToken = TidalToken B.ByteString deriving Show
instance ServiceToken TidalToken where
  requestTokenOpts (TidalToken token) =
    noTokenOpts <> oAuth2Bearer token
    
data Tidal = Search Page T.Text

tidal (Search p s) = do
  req GET url NoReqBody jsonResponse 
  where
    opts = noTokenOpts 
    url = tidalAPIUrl /: "search"


