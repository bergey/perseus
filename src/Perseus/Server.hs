module Main where

import Perseus.Api

import Control.Monad.IO.Class
import Data.Text
import Data.Time (UTCTime)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

server :: Server SampleApi
server = read :<|> write where
  read metric = do
    value <- liftIO $ readFile (metricFilename metric)
    return $ metric ++ "=" ++ value ++ "\n"
    -- return $ PostOk metric value
  write metric value = do
    liftIO $ writeFile (metricFilename metric) value
    -- return $ GetOk metric value
    return $ "OK, wrote " ++ metric ++ "=" ++ value ++ "\n"

app :: Application
app = serve sampleApi server

main :: IO ()
main = run 8081 app

path :: FilePath
path = "data/"

metricFilename :: String -> FilePath
metricFilename metric = path ++ metric
