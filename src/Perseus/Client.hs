{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Perseus.Api
import Perseus.Types

import System.Environment
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case parseRequest args of
    Nothing -> die help
    Just req -> do
      manager <- newManager defaultManagerSettings
      resp <- runClientM (perseus req) (ClientEnv manager hostInfo)
      either (die . show) putStrLn resp

perseus :: Request -> ClientM String
perseus = \case
  Post metric value -> do
    writeSample metric value
  Get metric -> do
    readSample metric

readSample :<|> writeSample = client sampleApi

hostInfo :: BaseUrl
hostInfo = BaseUrl Http "localhost" 8081 ""

data Request =
  Post String String
  | Get String
  deriving Show

parseRequest :: [String] -> Maybe Request
parseRequest = \case
    ["put", metric, value] -> Just $ Post metric value
    ["get", metric ] -> Just $ Get metric
    _ -> Nothing

help :: String
help = "usage:\nperseus-client put KEY VALUE\nperseus-client get KEY"
