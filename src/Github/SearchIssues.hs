{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Github.SearchIssues
  ( SearchIssuesQueryParams
  , searchIssues
  , searchIssuesRequest
  , parseSearchIssuesResponses
  ) where

import Config.Config
import Control.Lens (toListOf, view)
import Control.Monad.Catch
import Data.Aeson (Value)
import Data.Aeson.Lens (_Array, _String, key)
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.Configurator
import Data.Configurator.Types (Config, Configured)
import Data.Either
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Read (decimal)
import GHC.Generics (Generic)
import Github.Requests
  ( MonadEnv
  , MonadRequest
  , Params(..)
  , Response
  , getWithParams
  )
import qualified Network.Wreq as WREQ
import Network.Wreq (asValue, params, responseBody)
import Prelude hiding (lookup)

data SearchIssuesQueryParams =
  SearchIssuesQueryParams
    { q :: Q
    , sort :: Maybe Sort
    , order :: Maybe Order
    , perPage :: Maybe PerPage
    , page :: Maybe Page
    }
  deriving (Show, Eq)

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

instance Params SearchIssuesQueryParams where
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

data SearchIssuesResponseItem =
  SearchIssuesResponseItem
    { fullName :: FullName
    , title :: Title
    , number :: PRNumber
    , createdAt :: CreatedAt
    , login :: Login
    , htmlUrl :: HtmlUrl
    }
  deriving (Show, Eq)

newtype FullName =
  FullName T.Text
  deriving (Eq)

instance Show FullName where
  show (FullName x) = show x

newtype Title =
  Title T.Text
  deriving (Eq)

instance Show Title where
  show (Title x) = show x

newtype PRNumber =
  PRNumber Int
  deriving (Eq)

instance Show PRNumber where
  show (PRNumber x) = show x

newtype CreatedAt =
  CreatedAt T.Text
  deriving (Eq)

instance Show CreatedAt where
  show (CreatedAt x) = show x

newtype Login =
  Login T.Text
  deriving (Eq)

instance Show Login where
  show (Login x) = show x

newtype HtmlUrl =
  HtmlUrl T.Text
  deriving (Eq)

instance Show HtmlUrl where
  show (HtmlUrl x) = show x

class Monad m =>
      SearchContext m
  where
  req :: (Configured a) => Config -> T.Text -> m a
  look :: (Configured a) => Config -> T.Text -> m (Maybe a)

instance SearchContext IO where
  req = Data.Configurator.require
  look = Data.Configurator.lookup

type SearchIssuesResponses = Either String [SearchIssuesResponseItem]

searchIssues ::
     (SearchContext m, MonadRequest m, MonadEnv m, MonadThrow m)
  => Config
  -> m SearchIssuesResponses
searchIssues = parseSearchIssuesResponses . searchIssuesRequest

searchIssuesRequest ::
     (SearchContext m, MonadRequest m, MonadEnv m) => Config -> m Response
searchIssuesRequest cfg =
  cfg `req` "github.issues.q" >>= \q' ->
    cfg `look` "github.issues.sort" >>= \sort' ->
      cfg `look` "github.issues.order" >>= \order' ->
        cfg `look` "github.issues.perPage" >>= \perPage' ->
          cfg `look` "github.issues.page" >>= \page' ->
            getWithParams
              "https://api.github.com/search/issues"
              SearchIssuesQueryParams
                { q = Q (T.pack q')
                , sort = fmap (Sort . T.pack) sort'
                , order = fmap (Order . T.pack) order'
                , perPage = fmap PerPage perPage'
                , page = fmap Page page'
                }

parseSearchIssuesResponses ::
     (SearchContext m, MonadRequest m, MonadEnv m, MonadThrow m)
  => m Response
  -> m SearchIssuesResponses
parseSearchIssuesResponses r =
  r >>= \case
    Right x ->
      asValue x >>=
      (\y ->
         return
           (Right
              (map parseSearchIssuesResponse $
               toListOf (responseBody . key "items" . _Array . traverse) y)))
    Left err -> return (Left err)

parseSearchIssuesResponse :: Value -> SearchIssuesResponseItem
parseSearchIssuesResponse item =
  SearchIssuesResponseItem
    { fullName = FullName $ parseFullName $ parseHtmlUrl item
    , title = Title $ parseTitle item
    , number = PRNumber $ parsePRNumber $ parseHtmlUrl item
    , createdAt = CreatedAt $ parseCreatedAt item
    , login = Login $ parseLogin item
    , htmlUrl = HtmlUrl $ parseHtmlUrl item
    }
  where
    parseTitle = view (key "title" . _String)
    parseCreatedAt = view (key "created_at" . _String)
    parseLogin = view (key "user" . key "login" . _String)
    parseHtmlUrl = view (key "html_url" . _String)
    parseFullName url =
      T.intercalate "/" [T.splitOn "/" url !! 3, T.splitOn "/" url !! 4]
    parsePRNumber url =
      case Data.Text.Read.decimal $ last $ T.splitOn "/" url of
        Left _ -> -1 -- TODO: handle failure better here
        Right x -> fst x
