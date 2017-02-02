module Model where

import Network.HTTP.Affjax (URL)

newtype BuildType = BuildType
  { url :: URL
  , name :: String
  , project :: String
  }
