module Main where

import Control.Monad.Aff.Console as AC
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (Aff, later', runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
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

type AppStateData = String
type AppState     = Ref AppStateData

initState :: forall e. Eff ( "ref" :: REF | e) (Ref String)
initState = newRef "{ \"buildType\" : [] }"

getBuildTypes :: forall e. Aff ( "ajax" :: AJAX | e) String
getBuildTypes =
  let req = AX.defaultRequest
              {
                headers = [Accept applicationJSON]
              -- , url = "http://localhost:8080/sample"
              , url = "http://scifbuild01:81/guestAuth/app/rest/buildTypes"
              }
  in  AX.affjax req <#> _.response

-- Endpoints
getBuildTypesHandler :: forall e. AppState -> Handler (ajax :: AJAX, ref :: REF | e)
getBuildTypesHandler state = do
  setResponseHeader "Content-Type" applicationJSON
  setResponseHeader "Access-Control-Allow-Origin" "*"
  (liftEff $ readRef state) >>= send

sample :: forall e. Handler (fs :: FS, err :: EXCEPTION | e)
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

appSetup :: forall e. AppState -> App (ref :: REF, console :: CONSOLE , ajax :: AJAX, err :: EXCEPTION, fs :: FS | e)
appSetup state = do
  liftEff $ log "Setting up"
  setProp "json spaces" 4.0
  use               logger
  get "/buildTypes" (getBuildTypesHandler state)
  get "/sample"     sample
  useOnError        errorHandler

main :: forall e. Eff (ref :: REF, express :: EXPRESS,
                       console :: CONSOLE, process :: PROCESS,
                       ajax :: AJAX, err :: EXCEPTION,
                       fs :: FS | e)
                      Server
main = do
  port <- liftEff $ (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  state <- initState
  runAff logShow logShow $ setInterval 2000 $ do
    AC.log "Updating build types"
    str <- getBuildTypes
    liftEff $ writeRef state str
    AC.log "Updated build types"
  liftEff (listenHttp (appSetup state) port \_ ->
    log $ "Listening on " <> show port)

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a
