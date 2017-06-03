module Main where

import Perseus.Api

import Data.Text
import Data.Time (UTCTime)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

server1 :: Server SampleApi
server1 = read :<|> write where
  read metric = return $ "Hello, " ++ metric ++ "\n"
  write metric = return $ "unknown metric: " ++ metric ++ "\n"

sampleApi :: Proxy SampleApi
sampleApi = Proxy

app :: Application
app = serve sampleApi server1

main :: IO ()
main = run 8081 app
