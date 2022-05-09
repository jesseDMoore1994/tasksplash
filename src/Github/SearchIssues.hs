{-# LANGUAGE OverloadedStrings #-}

module Github.SearchIssues
  ( searchIssues
  ) where

import Config.Config
import qualified Data.Configurator as DC
import Data.Configurator.Types
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Github.Requests (Params(..), getWithParams)

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

searchIssues :: Config -> IO ()
searchIssues cfg =
  (getParamsFromConfig cfg :: IO SearchIssuesQuery) >>=
  getWithParams "https://api.github.com/search/issues" >>=
  print
