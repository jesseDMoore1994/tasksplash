{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Github
  ( SearchIssuesQuery
  , asKvList
  , kvsAsString
  , getIssues
  , getUrl
  , getParamsFromConfig
  ) where

import Config
import Control.Lens
import Data.Aeson.Lens (_String, key)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.Configurator as DC
import Data.Configurator.Types
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Network.URI.Encode
import Network.Wreq
import System.Posix.Env.ByteString

class Params a where
  getParamsFromConfig :: Config -> IO a
  asKvList :: a -> [(T.Text, T.Text)]

kvAsString :: (T.Text, T.Text) -> String
kvAsString (k, v) = "(" ++ show k ++ ", " ++ show v ++ ")"

kvsAsString :: [(T.Text, T.Text)] -> String
kvsAsString x = "[" ++ L.intercalate "," (map kvAsString x) ++ "]"

newtype Q =
  Q T.Text
  deriving (Eq)

instance Show Q where
  show (Q x) = show x

newtype Sort =
  Sort T.Text
  deriving (Eq)

instance Show Sort where
  show (Sort x) = show x

newtype Order =
  Order T.Text
  deriving (Eq)

instance Show Order where
  show (Order x) = show x

newtype PerPage =
  PerPage Int
  deriving (Eq)

instance Show PerPage where
  show (PerPage x) = show x

newtype Page =
  Page Int
  deriving (Eq)

instance Show Page where
  show (Page x) = show x

data SearchIssuesQuery =
  SearchIssuesQuery
    { q :: Q
    , sort :: Maybe Sort
    , order :: Maybe Order
    , perPage :: Maybe PerPage
    , page :: Maybe Page
    }
  deriving (Show, Eq)

instance Params SearchIssuesQuery where
  getParamsFromConfig cfg = do
    q' <- (cfg `DC.require` "github.issues.q") :: IO String
    sort' <- (cfg `DC.lookup` "github.issues.sort") :: IO (Maybe String)
    order' <- (cfg `DC.lookup` "github.issues.order") :: IO (Maybe String)
    perPage' <- (cfg `DC.lookup` "github.issues.perPage") :: IO (Maybe Int)
    page' <- (cfg `DC.lookup` "github.issues.page") :: IO (Maybe Int)
    return
      SearchIssuesQuery
        { q = Q (T.pack q')
        , sort = fmap (Sort . T.pack) sort'
        , order = fmap (Order . T.pack) order'
        , perPage = fmap PerPage perPage'
        , page = fmap Page page'
        }
  asKvList ggp =
    [ (x, fromJust y)
    | (x, y) <- filter (isJust . snd) [q', sort', order', perPage', page']
    ]
    where
      q' = ("q", Just (T.pack . filter (/= '"') . show $ q ggp))
      sort' = ("sort", T.pack . show <$> sort ggp)
      order' = ("order", T.pack . show <$> order ggp)
      perPage' = ("per_page", T.pack . show <$> perPage ggp)
      page' = ("page", T.pack . show <$> page ggp)

readTokenFromEnv :: IO B.ByteString
readTokenFromEnv =
  getEnv "GITHUB_TOKEN" >>=
  (\case
     Just token -> return token
     Nothing ->
       error "Authentication token variable GITHUB_TOKEN not found in env")

getOpts :: Params a => a -> IO Options
getOpts get_params =
  readTokenFromEnv >>=
  (\token ->
     return
       (defaults & header "Accept" .~ ["application/vnd.github.v3+json"] & auth ?~
        oauth2Token token &
        params .~
        asKvList get_params))

getUrl :: Params a => String -> a -> IO T.Text
getUrl url params =
  getOpts params >>=
  (\options ->
     getWith options url >>=
     (\response -> return (response ^. responseBody . key "url" . _String)))

getWithParams :: Params a => String -> a -> IO (Response BL.ByteString)
getWithParams url params = getOpts params >>= (`getWith` url)

getIssues :: Config -> IO ()
getIssues cfg =
  (getParamsFromConfig cfg :: IO SearchIssuesQuery) >>=
  getWithParams "https://api.github.com/search/issues" >>=
  print
