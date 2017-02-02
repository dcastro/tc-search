module Model where

import Prelude
import Data.Newtype (class Newtype)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Network.HTTP.Affjax (URL)

newtype BuildType = BuildType
  { url :: URL
  , name :: String
  , project :: String
  }

newtype BuildTypes = BuildTypes (Array BuildType)
derive instance newtypeBuildTypes :: Newtype BuildTypes _

instance decodeBuildType :: DecodeJson BuildType where
  decodeJson json = do
    obj <- decodeJson json
    url <- obj .? "webUrl"
    name <- obj .? "name"
    project <- obj .? "projectName"
    pure $ BuildType { url, name, project }

instance decodeBuildTypes :: DecodeJson BuildTypes where
  decodeJson json = do
    obj <- decodeJson json
    xs <- obj .? "buildType"
    pure $ BuildTypes xs
