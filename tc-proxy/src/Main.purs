module Main where

import Network.HTTP.Affjax as AX
import Control.Alt ((<|>))
import Control.Logger (log)
import Control.Monad.Aff (Aff, attempt, later', runAff)
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.MediaType.Common (applicationJSON)
import Loggers (consoleLogger, fileLogger)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Node.Encoding (Encoding(..))
import Node.Express.App (App, listenHttp, listenPipe, useOnError, get, use, setProp)
import Node.Express.Handler (Handler, HandlerM, next)
import Node.Express.Request (getOriginalUrl, getRemoteIp, getRequestHeader)
import Node.Express.Response (send, sendJson, setResponseHeader, setStatus)
import Node.Express.Types (EXPRESS)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.HTTP (Server)
import Node.Process (PROCESS, lookupEnv)
import Prelude hiding (apply)

type AppStateData = Maybe String
type AppState     = Ref AppStateData

getBuildTypes :: forall e m. (MonadAff ("ajax" :: AJAX | e) m) => m String
getBuildTypes =
  let req = AX.defaultRequest
              {
                headers = [Accept applicationJSON]
              -- , url = "http://localhost/tcproxy/sample"
              -- , url = "http://localhost:8080/tcproxy/sample"
              -- , url = "http://scifbuild01:81/guestAuth/app/rest/buildTypes"
              , url = "http://build.navimedix.com/guestAuth/app/rest/buildTypes"
              }
  in  liftAff $ AX.affjax req <#> _.response

-- Endpoints
getBuildTypesHandler :: forall e. AppState -> Handler (ajax :: AJAX, ref :: REF | e)
getBuildTypesHandler state = do
  setResponseHeader "Content-Type" applicationJSON
  setResponseHeader "Access-Control-Allow-Origin" "*"
  s  <- liftEff (readRef state)
  s' <- fromMaybe getBuildTypes (pure <$> s)
  send (s' :: String)

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
  url <- getOriginalUrl
  ip  <- getIp
  log' $ url <> " " <> ip
  next

getIp :: forall e. HandlerM (express :: EXPRESS | e) String
getIp = getRemoteIp >>= \ip ->
  if (("" <> ip) /= "undefined")
    then pure ip
    else fromMaybe "N/A" <$> getRequestHeader "x-forwarded-for"

errorHandler :: forall e. Error -> Handler e
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

appSetup :: forall e. AppState -> App (ref :: REF, console :: CONSOLE, ajax :: AJAX, err :: EXCEPTION, fs :: FS, now :: NOW | e)
appSetup state = do
  log' "Setting up"
  setProp "json spaces" 4.0
  use                       loggerHandler
  get "/tcproxy/buildTypes" (getBuildTypesHandler state)
  get "/tcproxy/sample"     sample
  useOnError                errorHandler

main :: forall e. Eff (ref :: REF, express :: EXPRESS,
                       console :: CONSOLE, process :: PROCESS,
                       ajax :: AJAX, err :: EXCEPTION,
                       fs :: FS, now :: NOW | e)
                      Server
main = do
  state <- newRef Nothing
  runAff log' log' $ setInterval 2000 $ catchAndLog $ do
    str <- getBuildTypes
    liftEff $ writeRef state (Just str)

  let app = appSetup state
  let callback port = \_ -> log' $ "Listening on " <> port
  envPort <- lookupEnv "PORT"

  liftEff $
    (envPort >>= fromString <#>   \p -> listenHttp app p (callback $ show p))
    <|> (envPort <#>              \p -> listenPipe app p (callback p))
    # fromMaybe'                  \_ -> listenHttp app 8080 (callback "8080")

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a

catchAndLog :: forall a e. Aff (fs :: FS, err :: EXCEPTION, now :: NOW, console :: CONSOLE | e) a -> Aff (fs :: FS, err :: EXCEPTION, now :: NOW, console :: CONSOLE | e) (Maybe a)
catchAndLog ma =
  attempt ma >>= \x ->
    case x of
        Left err -> liftEff (log' err) $> Nothing
        Right a -> pure (Just a)
