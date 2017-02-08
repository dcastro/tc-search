module Main where


import Network.HTTP.Affjax as AX
import Control.Logger (log)
import Control.Monad.Aff (Aff, attempt, later', runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (applicationJSON)
import Loggers (consoleLogger, fileLogger)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Node.Encoding (Encoding(..))
import Node.Express.App (App, listenHttp, useOnError, get, use, setProp)
import Node.Express.Handler (Handler, next)
import Node.Express.Request (getOriginalUrl, getRemoteIp)
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
logger = fileLogger "log.log" <> consoleLogger
log' = log logger

loggerHandler :: forall e. Handler (console :: CONSOLE, ref :: REF, fs :: FS, err :: EXCEPTION, now :: NOW | e)
loggerHandler = do
  url   <- getOriginalUrl
  host  <- getRemoteIp
  log' $ url <> " " <> host
  next

errorHandler :: forall e. Error -> Handler e
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

appSetup :: forall e. AppState -> App (ref :: REF, console :: CONSOLE , ajax :: AJAX, err :: EXCEPTION, fs :: FS, now :: NOW | e)
appSetup state = do
  log' "Setting up"
  setProp "json spaces" 4.0
  use               loggerHandler
  get "/buildTypes" (getBuildTypesHandler state)
  get "/sample"     sample
  useOnError        errorHandler

main :: forall e. Eff (ref :: REF, express :: EXPRESS,
                       console :: CONSOLE, process :: PROCESS,
                       ajax :: AJAX, err :: EXCEPTION,
                       fs :: FS, now :: NOW | e)
                      Server
main = do
  port <- liftEff $ (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  state <- initState
  runAff log' log' $ setInterval 2000 $ catchAndLog $ do
    str <- getBuildTypes
    liftEff $ writeRef state str
  liftEff (listenHttp (appSetup state) port \_ ->
    log' $ "Listening on " <> show port)

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a

catchAndLog :: forall a e. Aff (fs :: FS, err :: EXCEPTION, now :: NOW | e) a -> Aff (fs :: FS, err :: EXCEPTION, now :: NOW | e) (Maybe a)
catchAndLog ma =
  attempt ma >>= \x ->
    case x of
        Left err -> liftEff (log' err) $> Nothing
        Right a -> pure (Just a)
