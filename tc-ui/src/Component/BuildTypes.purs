module Component.BuildTypes where

import Prelude
import Data.String as S
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Network.HTTP.Affjax as AX
import Control.Apply (lift2)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (hash, href, setHash)
import DOM.HTML.Window (location)
import Data.Argonaut (decodeJson)
import Data.Array (all, filter, length, sortBy)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (un)
import Data.String.Utils (includes, words)
import Global (decodeURIComponent, encodeURIComponent)
import Halogen (fromEff)
import Halogen.HTML (className)
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events.Handler (preventDefault)
import Model (BuildType(..), BuildTypes(..), getName, getProject)
import Network.HTTP.Affjax (AJAX)

type State =
  { searchText :: String
  , result :: Maybe Result
  , href :: String
  }

type Result = Either String (Array BuildType)

type Effects eff = (ajax :: AX.AJAX, console :: CONSOLE, dom :: DOM | eff)

data Query a
  = Initialize a
  | UpdateText String a
  | GenLink a
  | CopyLink a

initialState :: String -> State
initialState = { searchText: "", result: Nothing , href: _ }

ui :: forall eff. H.Component State Query (Aff (Effects eff))
ui = H.lifecycleComponent
  { render
  , eval
  , initializer: Just $ H.action Initialize
  , finalizer: Nothing
  }

render :: State -> H.ComponentHTML Query
render s =
  HH.div_
    [ HH.div
        [ HP.class_ $ className "navbar-fixed"]
        [ HH.nav_
            [ HH.div
                [ HP.class_ $ className "nav-wrapper indigo" ]
                [ HH.form
                    [ HE.onSubmit $ (\_ -> preventDefault $> Nothing) ]
                    [ HH.div
                        [ HP.class_ $ className "input-field" ]
                        [ HH.input
                            [ HP.id_ "search-text"
                            , HP.inputType HP.InputSearch
                            , HP.autofocus true
                            , HP.value s.searchText
                            , HE.onValueInput (HE.input UpdateText)
                            ]
                        , HH.label
                            [ HP.class_ $ className "label-icon"
                            , HP.for "search-text"
                            ]
                            [ HH.i
                                [ HP.class_ $ className "material-icons" ]
                                [ HH.text "search" ]
                            ]
                        , HH.i
                            [ HP.class_ $ className "material-icons" ]
                            [ HH.text "close" ]
                        ]
                    ]
                ]
            ]
        ]
    , HH.main
        [ HP.class_ $ className "container" ]
        [ case s.result of
            Nothing         -> loading
            Just (Left err) -> showError err
            Just (Right xs) -> renderBuildTypes $ filter (isMatch s.searchText) xs
        ]
    , renderModal s.href
    ]

loading :: forall p i. HTML p i
loading = HH.div
            [ HP.class_ $ className "progress" ]
            [ HH.div
                [ HP.class_ $ className "indeterminate" ]
                [ ]
            ]

showError :: forall p i. String ->  HTML p i
showError err =
  HH.div
    [ HP.classes $ className <$> [ "card-panel", "red" ] ]
    [ HH.span
        [ HP.class_ $ className "white-text" ]
        [ HH.text err ]
    ]

renderBuildTypes :: Array BuildType -> H.ComponentHTML Query
renderBuildTypes xs =
  HH.div_
    [ HH.div
        [ HP.class_ $ className "row valign-wrapper" ]
        [ HH.div
            [ HP.class_ $ className "col s6" ]
            [ renderResultCount xs ]
        , HH.div
            [ HP.class_ $ className "col s6 right-align" ]
            [ HH.p_
                [ HH.a
                    [ HP.class_ $ className "btn-floating waves-effect waves-light teal tooltipped"
                    , HP.id_ "gen-link"
                    , HE.onClick $ HE.input_ GenLink
                    , HP.href "#copyModal"
                    ]
                    [ HH.i [ HP.class_ $ className "material-icons" ] [ HH.text "link" ] ]
                ]
            ]
        ]
    , HH.div
        [ HP.classes $ className <$> ["collection", "z-depth-3"] ]
        (renderBuildType <$> xs)
    ]

renderResultCount :: forall q. Array BuildType -> H.ComponentHTML q
renderResultCount xs =
  let count   = length xs
      matches = if count == 1 then "match" else "matches"
  in  HH.p_ [ HH.text $ "Found " <> show count <> " " <> matches ]

renderBuildType :: BuildType -> H.ComponentHTML Query
renderBuildType (BuildType x) =
  HH.a
    [ HP.href x.url, HP.target "_blank", HP.class_ $ className "collection-item" ]
    [ HH.text $ x.project <> " > " <> x.name ]

renderModal :: String -> H.ComponentHTML Query
renderModal href =
  HH.div
    [ HP.id_ "copyModal", HP.class_ $ className "modal" ]
    [ HH.div
        [ HP.class_ $ className "modal-content row" ]
        [ HH.div
            [ HP.class_ $ className "col s11" ]
            [ HH.span
                [ HP.id_ "copy-text-area" ]
                [ HH.text href ]
            ]
        , HH.div
            [ HP.class_ $ className "col s1 right-align" ]
            [ HH.a
                [ HP.class_ $ className "btn-floating waves-effect waves-light teal"
                , HP.id_ "copy-link"
                , HE.onClick $ HE.input_ CopyLink
                ]
                [ HH.i [ HP.class_ $ className "material-icons" ] [ HH.text "content_copy" ]
                ]
            ]
        ]
    ]

eval :: forall eff. Query ~> H.ComponentDSL State Query (Aff (Effects eff))
eval (Initialize next) = do
  searchText  <- H.fromEff $ window >>= location >>= hash <#> S.dropWhile (_ == '#') >>> decodeURIComponent
  next'       <- eval (UpdateText searchText next)

  result <- H.fromAff $ getBuildTypes
  H.modify _ { result = Just result }
  H.fromEff (initTooltip *> initModals)
  pure next'
eval (UpdateText s next)  = H.modify _ { searchText = s } $> next
eval (GenLink next)       = do
  s <- H.gets _.searchText
  H.fromEff $ window >>= location >>= setHash (encodeURIComponent s)
  href <- H.fromEff $ window >>= location >>= href
  H.modify _ { href = href }
  pure next
eval (CopyLink next) = do
  href    <- H.gets _.href
  success <- fromEff $ clipboard href
  fromEff $ showToast $ if success then "Copied URL" else "Unable to copy URL"
  pure next

foreign import initTooltip  :: forall eff. Eff (dom :: DOM | eff) Unit
foreign import initModals   :: forall eff. Eff (dom :: DOM | eff) Unit

foreign import clipboard    :: forall eff. String -> Eff (dom :: DOM | eff) Boolean
foreign import showToast    :: forall eff. String -> Eff (dom :: DOM | eff) Unit

getBuildTypes :: forall e. Aff (ajax :: AJAX | e) (Either String (Array BuildType))
getBuildTypes = do
  response <- attempt $ AX.get "http://localhost:8080/buildTypes"
  let result =
        response
        # bimap show _.response
        >>= decodeJson
        <#> un BuildTypes
        >>> sortBuildTypes
  pure result

sortBuildTypes :: Array BuildType -> Array BuildType
sortBuildTypes = sortBy (comparing getProject <> comparing getName)

fullText :: BuildType -> String
fullText = lift2 (<>) getProject getName >>> S.toLower

isMatch :: String -> BuildType -> Boolean
isMatch str x = all (_ `includes` fullText x) $ words $ S.toLower str
