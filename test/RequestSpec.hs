{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module RequestSpec
  ( tests
  ) where

import Control.Exception (evaluate)
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Github.Requests (MonadEnv(..), readTokenFromEnv)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

newtype FoundInEnv a =
  FoundInEnv
    { unFoundInEnv :: IO a
    }
  deriving (Functor, Applicative, Monad)

instance MonadEnv FoundInEnv where
  getEnv val = FoundInEnv $ return (Just "aaa")

newtype NotFoundInEnv a =
  NotFoundInEnv
    { unNotFoundInEnv :: IO a
    }
  deriving (Functor, Applicative, Monad)

instance MonadEnv NotFoundInEnv where
  getEnv val = NotFoundInEnv $ return Nothing

readToken :: Property
readToken =
  property $ do
    x <- liftIO $ unFoundInEnv readTokenFromEnv
    x === "aaa"

readTokenFailure :: Property
readTokenFailure =
  property $ do
    x <- liftIO $ unNotFoundInEnv readTokenFromEnv
    x === "aaa"

tests :: IO Bool
tests =
  checkSequential $
  Group
    "RequestSpec"
    [ ("readToken", readToken)
    --, ("readTokenFailure", readTokenFailure)
    ]
