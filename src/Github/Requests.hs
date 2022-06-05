{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Github.Requests
  ( Params(asKvList)
  , getWithParams
  , MonadEnv(getEnv)
  , MonadRequest
  , readTokenFromEnv
  , Response
  ) where

import Control.Lens
import Control.Monad.Trans.Class
import Data.Aeson.Lens (_String, key)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Internal as BL
import Data.Configurator.Types
import Data.Functor
import qualified Data.Text as T
import qualified Network.Wreq as WREQ
import qualified System.Posix.Env.ByteString as ENV

class Params a where
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

type Token = Either String B.ByteString

readTokenFromEnv :: MonadEnv m => m Token
readTokenFromEnv =
  getEnv "GITHUB_TOKEN" >>=
  (\case
     Just x -> return $ Right x
     _ -> return $ Left "Cannot get GITHUB_TOKEN from env!")

type Options = Either String WREQ.Options

getOpts :: (MonadEnv m, Params a) => a -> m Options
getOpts get_params =
  let construct get_params token =
        (WREQ.defaults &
         WREQ.header "Accept" .~ ["application/vnd.github.v3+json"] &
         WREQ.auth ?~ WREQ.oauth2Token token &
         WREQ.params .~ asKvList get_params)
   in readTokenFromEnv >>=
      (\case
         Right token -> return (Right (construct get_params token))
         Left err -> return (Left err))

type Response = Either String (WREQ.Response BL.ByteString)

getWithParams ::
     (MonadRequest m, MonadEnv m, Params a) => String -> a -> m Response
getWithParams url params =
  getOpts params >>= \case
    Right x -> getWith x url <&> Right
    Left err -> return (Left err)
