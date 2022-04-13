{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Github
  ( getIssues
  , getUrl
  ) where

import Control.Lens
import Data.Aeson.Lens (_String, key)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import Network.Wreq
import System.Posix.Env.ByteString

class GetParams a where
  getGetParams :: a -> a
  asKvList :: a -> [(T.Text, T.Text)]

newtype Q =
  Q TI.Text
  deriving (Show, Eq)

newtype Sort =
  Sort TI.Text
  deriving (Show, Eq)

newtype Order =
  Order TI.Text
  deriving (Show, Eq)

newtype PerPage =
  PerPage Int
  deriving (Show, Eq)

newtype Page =
  Page Int
  deriving (Show, Eq)

data SearchIssuesQuery =
  SearchIssuesQuery
    { q :: Q
    , sort :: Maybe Sort
    , order :: Maybe Order
    , perPage :: Maybe PerPage
    , page :: Maybe Page
    }
  deriving (Show, Eq)

instance GetParams SearchIssuesQuery where
  getGetParams ggp = ggp
  asKvList ggp =
    filter (\x -> snd x /= "Nothing") [q', sort', order', perPage', page']
    where
      q' = ("q", T.pack . show $ q ggp)
      sort' = ("sort", T.pack . show $ sort ggp)
      order' = ("order", T.pack . show $ order ggp)
      perPage' = ("perPAge", T.pack . show $ perPage ggp)
      page' = ("page", T.pack . show $ page ggp)

readTokenFromEnv :: IO B.ByteString
readTokenFromEnv =
  getEnv "GITHUB_TOKEN" >>=
  (\case
     Just token -> return token
     Nothing ->
       error "Authentication token variable GITHUB_TOKEN not found in env")

getOpts :: GetParams a => a -> IO Options
getOpts get_params =
  readTokenFromEnv >>=
  (\token ->
     return
       (defaults & header "Accept" .~ ["application/vnd.github.v3+json"] & auth ?~
        oauth2Token token &
        params .~
        asKvList get_params))

getUrl :: GetParams a => String -> a -> IO TI.Text
getUrl url params =
  getOpts params >>=
  (\options ->
     getWith options url >>=
     (\response -> return (response ^. responseBody . key "url" . _String)))

getWithParams :: GetParams a => String -> a -> IO (Response BL.ByteString)
getWithParams url params = getOpts params >>= (`getWith` url)

getIssues :: IO ()
getIssues =
  getWithParams
    "https://api.github.com/search/issues"
    SearchIssuesQuery
      { q = Q "isAuthor:jmoore"
      , sort = Nothing
      , order = Nothing
      , perPage = Nothing
      , page = Nothing
      } >>=
  print
