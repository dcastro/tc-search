module Model where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Function.Memoize (class Tabulate, tabulate)
import Data.Newtype (class Newtype, unwrap)
import Network.HTTP.Affjax (URL)

newtype BuildType = BuildType
  { url :: URL
  , name :: String
  , project :: String
  }

getUrl      = (unwrap >>> _.url)
getName     = (unwrap >>> _.name)
getProject  = (unwrap >>> _.project)

newtype BuildTypes = BuildTypes (Array BuildType)

derive instance newtypeBuildType :: Newtype BuildType _
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

instance tabulateBuildType :: Tabulate BuildType where
  tabulate f = let f' = tabulate \url -> tabulate \name -> tabulate \project -> f (BuildType {url, name, project})
               in \(BuildType { url, name, project }) ->
                  do  g <- f' url
                      h <- g name
                      h project
