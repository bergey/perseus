{-# LANGUAGE LambdaCase #-}

module Main where

import Perseus.Types

import System.Environment

main :: IO ()
main = do
  req <- parseRequest <$> getArgs
  resp <- traverse perseus req
  putStrLn $ logResult resp

perseus :: Request -> IO Response
perseus = \case
  Post metric value -> do
    writeFile (metricFilename metric) value
    return $ PostOk metric value
  Get metric -> do
    value <- readFile (metricFilename metric)
    return $ GetOk metric value

path :: FilePath
path = "data/"

data Request =
  Post String String
  | Get String

data Response =
  PostOk String String
  | GetOk String String

data Error = BadRequest

metricFilename :: String -> FilePath
metricFilename metric = path ++ metric

parseRequest :: [String] -> Either Error Request
parseRequest = \case
    ["put", metric, value] -> Right $ Post metric value
    ["get", metric ] -> Right $ Get metric
    _ -> Left BadRequest

logResponse :: Response -> String
logResponse = \case
  PostOk metric value -> "OK, wrote " ++ metric ++ "=" ++ value
  GetOk metric value -> "GOT " ++ metric ++ "=" ++ value

logError :: Error -> String
logError BadRequest = "Bad Request"

logResult :: Either Error Response -> String
logResult = either logError logResponse
