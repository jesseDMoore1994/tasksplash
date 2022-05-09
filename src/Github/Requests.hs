{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Github.Requests
  ( Params(getParamsFromConfig, asKvList)
  , getWithParams
  , MonadEnv(getEnv)
  , MonadRequest
  , readTokenFromEnv
  ) where

import Control.Lens
import Data.Aeson.Lens (_String, key)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Internal as BL
import Data.Configurator.Types
import qualified Data.Text as T
import qualified Network.Wreq as WREQ
import qualified System.Posix.Env.ByteString as ENV

class Params a where
  getParamsFromConfig :: Config -> IO a
  asKvList :: a -> [(T.Text, T.Text)]

class Monad m =>
      MonadEnv m
  where
  getEnv :: B.ByteString -> m (Maybe B.ByteString)

instance MonadEnv IO where
  getEnv = ENV.getEnv

class Monad m =>
      MonadRequest m
  where
  getWith :: WREQ.Options -> String -> m (WREQ.Response BL.ByteString)

instance MonadRequest IO where
  getWith = WREQ.getWith

readTokenFromEnv :: MonadEnv m => m B.ByteString
readTokenFromEnv =
  getEnv "GITHUB_TOKEN" >>=
  (\case
     Just token -> return token
     Nothing ->
       error "Authentication token variable GITHUB_TOKEN not found in env")

getOpts :: (MonadEnv m, Params a) => a -> m WREQ.Options
getOpts get_params =
  readTokenFromEnv >>=
  (\token ->
     return
       (WREQ.defaults &
        WREQ.header "Accept" .~ ["application/vnd.github.v3+json"] &
        WREQ.auth ?~ WREQ.oauth2Token token &
        WREQ.params .~ asKvList get_params))

getWithParams ::
     (MonadRequest m, MonadEnv m, Params a)
  => String
  -> a
  -> m (WREQ.Response BL.ByteString)
getWithParams url params = getOpts params >>= (`getWith` url)
