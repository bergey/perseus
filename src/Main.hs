{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.Environment
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Serialize.Text
import Data.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vector.Serialize
import Data.Word
import GHC.Generics

type Timestamp = Word64
type Float32 = Float
type Float64 = Double

data Sample = Sample {- UNBOX -} !Timestamp {-UNBOX -} !Float32
  deriving (Eq, Ord, Generic, Show)
-- TODO Should Ord be just by timestamp?

instance Serialize Sample
instance ToJSON Sample
instance FromJSON Sample

data Metric = Metric {
  name :: Text, -- TODO maybe this should just be a BS validated as UTF-8
  labels :: V.Vector (Text, Text)
  } deriving (Eq, Ord, Generic, Show)

instance ToJSON Metric
instance FromJSON Metric
instance Serialize Metric

data Series = Series !Metric !(V.Vector Sample)

data Incoming = Incoming !Metric !Sample

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

perseus :: Request -> IO Response
perseus = \case
  Post metric value -> do
    writeFile (metricFilename metric) value
    return $ PostOk metric value
  Get metric -> do
    value <- readFile (metricFilename metric)
    return $ GetOk metric value

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

main :: IO ()
main = do
  req <- parseRequest <$> getArgs
  resp <- traverse perseus req
  putStrLn $ logResult resp
