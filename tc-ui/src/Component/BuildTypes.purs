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
import DOM.HTML.Location (hash, setHash)
import DOM.HTML.Window (location)
import Data.Argonaut (decodeJson)
import Data.Array (all, filter, length, sortBy)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (un)
import Data.String.Utils (includes, words)
import Halogen.HTML (className)
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events.Handler (preventDefault)
import Model (BuildType(..), BuildTypes(..), getName, getProject)

type State =
  { searchText :: String
  , result :: Maybe Result
  }

type Result = Either String (Array BuildType)

type Effects eff = (ajax :: AX.AJAX, console :: CONSOLE, dom :: DOM | eff)

data Query a
  = Initialize a
  | UpdateText String a
  | GenLink a
  | NoAction a

initialState :: State
initialState = { searchText: "", result: Nothing }

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
                    [ HE.onSubmit $ (\e -> preventDefault *> HE.input_ NoAction e) ]
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
            [ HH.p_ [ HH.text $ "Found " <> show (length xs) <> " matches." ] ]
        , HH.div
            [ HP.class_ $ className "col s6 right-align" ]
            [ HH.p_
                [ HH.a
                    [ HP.class_ $ className "btn-floating waves-effect waves-light teal tooltipped"
                    , HP.id_ "gen-link"
                    , HE.onClick $ HE.input_ GenLink
                    ]
                    [ HH.i [ HP.class_ $ className "material-icons" ] [ HH.text "link" ] ]
                ]
            ]
        ]
    , HH.div
        [ HP.classes $ className <$> ["collection", "z-depth-3"] ]
        (renderBuildType <$> xs)
    ]

renderBuildType :: BuildType -> H.ComponentHTML Query
renderBuildType (BuildType x) =
  HH.a
    [ HP.href x.url, HP.target "_blank", HP.class_ $ className "collection-item" ]
    [ HH.text $ x.project <> " > " <> x.name ]

eval :: forall eff. Query ~> H.ComponentDSL State Query (Aff (Effects eff))
eval (Initialize next) = do
  searchText  <- H.fromEff $ window >>= location >>= hash <#> S.dropWhile (_ == '#')
  next'       <- eval (UpdateText searchText next)

  response <- H.fromAff $ attempt $ AX.get "http://localhost:8080/buildTypes"
  let result =
        response
        # bimap show _.response
        >>= decodeJson
        <#> un BuildTypes
        >>> sortBuildTypes
  H.modify (_ { result = Just result })
  H.fromEff initTooltip
  pure next'
eval (UpdateText s next)  = H.modify (_ { searchText = s }) *> pure next
eval (NoAction next)      = pure next
eval (GenLink next)       = do
  s <- H.gets _.searchText
  H.fromEff $ window >>= location >>= setHash s
  pure next

foreign import initTooltip :: forall eff. Eff (dom :: DOM | eff) Unit

sortBuildTypes :: Array BuildType -> Array BuildType
sortBuildTypes = sortBy (comparing getProject <> comparing getName)

fullText :: BuildType -> String
fullText = lift2 (<>) getProject getName >>> S.toLower

isMatch :: String -> BuildType -> Boolean
isMatch str xs = all (_ `includes` fullText xs) $ words $ S.toLower str
