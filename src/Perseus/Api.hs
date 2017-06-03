{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Perseus.Api where

import Data.Proxy
import Data.Text
import Data.Time (UTCTime)
import Servant.API

type SampleApi =
  "read" :> Capture "metric" String :> Get '[PlainText] String
  :<|> "write" :> Capture "metric" String :> Capture "value" String :> Post '[PlainText] String  -- TODO "" only

sampleApi :: Proxy SampleApi
sampleApi = Proxy
