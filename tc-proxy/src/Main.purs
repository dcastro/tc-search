module Main where

import Network.HTTP.Affjax as AX
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Eff.Ref (REF)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Node.Encoding (Encoding(..))
import Node.Express.App (App, listenHttp, useOnError, get, use, setProp)
import Node.Express.Handler (Handler, next)
import Node.Express.Request (getOriginalUrl, setUserData)
import Node.Express.Response (send, sendJson, setResponseHeader, setStatus)
import Node.Express.Types (EXPRESS)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.HTTP (Server)
import Node.Process (PROCESS, lookupEnv)
import Prelude hiding (apply)

getBuildTypes :: forall e. Handler (ajax :: AJAX |e)
getBuildTypes = do
  let req = AX.defaultRequest
              {
                headers = [Accept applicationJSON]
              , url = "http://localhost:8080/sample"
              -- , url = "http://scifbuild01:81/guestAuth/app/rest/buildTypes"
              }
  rsp <- liftAff $ AX.affjax req
  setResponseHeader "Content-Type" applicationJSON
  setResponseHeader "Access-Control-Allow-Origin" "*"
  send (rsp.response :: String)

sample :: forall e. Handler (fs :: FS, err :: EXCEPTION |e)
sample = do
  json <- liftEff $ readTextFile UTF8 "sample.json"
  send json

parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str

-- Monadic handlers
logger :: forall e. Handler (console :: CONSOLE, ref :: REF | e)
logger = do
  url   <- getOriginalUrl
  liftEff $ log (">>> " <> url)
  setUserData "logged" url
  next

errorHandler :: forall e. Error -> Handler e
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

appSetup :: forall e. App (ref :: REF, console :: CONSOLE , ajax :: AJAX, err :: EXCEPTION, fs :: FS | e)
appSetup = do
  liftEff $ log "Setting up"
  setProp "json spaces" 4.0
  use               logger
  get "/buildTypes" getBuildTypes
  get "/sample"     sample
  useOnError        errorHandler

main :: forall e. Eff (ref :: REF, express :: EXPRESS,
                       console :: CONSOLE, process :: PROCESS,
                       ajax :: AJAX, err :: EXCEPTION,
                       fs :: FS | e)
                      Server
main = do
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  listenHttp appSetup port \_ ->
    log $ "Listening on " <> show port
