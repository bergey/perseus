{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Perseus.Api
import Perseus.Types

import System.Environment
import Data.Aeson
import Data.Monoid
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Options.Applicative        as O
import qualified Options.Applicative.Types  as O
import Servant.API
import Servant.Client
import System.Exit

main :: IO ()
main = do
  config <- O.execParser helpParser
  manager <- newManager defaultManagerSettings
  resp <- runClientM (perseus (request config)) (ClientEnv manager (hostInfo config))
  either (die . show) putStrLn resp

perseus :: Request -> ClientM String
perseus = \case
  Post metric value -> do
    writeSample metric value
  Get metric -> do
    readSample metric

readSample :<|> writeSample = client sampleApi

hostInfo :: Config -> BaseUrl
hostInfo config = BaseUrl Http (host config) (port config) ""

data Request =
  Post String String
  | Get String
  deriving Show

data Config = Config {
  host :: String,
  port :: Int,
  request :: Request
  }

cliParser :: O.Parser Config
cliParser = Config
  <$> O.strOption (O.long "host" <> O.short 'h' <> O.value "localhost")
  <*> O.option O.auto (O.long "port" <> O.short 'p' <> O.value 8081)
  <*> requestParser
  where
    requestParser = O.subparser (
      O.command "put" (O.info putOptions (O.progDesc "write a key value pair"))
      <> O.command "get" (O.info getOptions (O.progDesc "get the value of the given key")))
    putOptions = Post
      <$> getParser
      <*> O.strOption (O.long "value" <> O.short 'v')
    getOptions = Get <$> getParser
    getParser = O.strOption (O.long "key" <> O.short 'k')

helpParser :: O.ParserInfo Config
helpParser = O.info (O.helper <*> cliParser)
  (O.fullDesc <> O.header "perseus-client - minimal CLI client for Perseus TSDB")
