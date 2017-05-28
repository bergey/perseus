{-# LANGUAGE DeriveGeneric #-}

module Main where

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
  labels :: V.Vector (Text, Text),
  samples :: V.Vector Sample
  } deriving (Eq, Ord, Generic, Show)

instance ToJSON Metric
instance FromJSON Metric
instance Serialize Metric

main :: IO ()
main = putStrLn "Hello, Haskell!"
